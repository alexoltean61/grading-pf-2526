{-
    Acest modul se ocupă cu conectarea la un server de SMTP pentru trimis mass email nesolicitat despre
    facultate........
-}
{-# LANGUAGE OverloadedStrings #-}

module Mailer where

import Data.String
import Data.Char (toLower)
import Data.Text ( pack, intercalate, concat )
import Network.Mail.Mime
import Network.Mail.SMTP ( sendMailWithLogin', connectSMTPS', login, renderAndSend, closeSMTP )
import System.Environment (getEnv)
import Control.Monad
import Control.Monad.Trans (liftIO)
import Control.Monad.Except ( ExceptT, MonadError(throwError) )
import Control.Monad.Writer
import Model
import Formula
import Data.ByteString (ByteString)

sendToStudent :: Laborator -> StudentPair -> ExceptT String IO ()
sendToStudent nrLab sp@(student, d) = do
    let to = genAddress student
    let subject = "Punctajul tău parțial la labul de PF"
    let plainBody = genPlainContent d
    let htmlBody  = genHtmlContent nrLab sp
    host <- liftIO $ getEnv "HOST" 
    username <- liftIO $ getEnv "USER"
    password <- liftIO $ getEnv "PASS"
    name <- liftIO $ pack <$> getEnv "NAME"
    selfAddr <- liftIO $ pack <$> getEnv "ADDR"
    let self = Address (Just name) selfAddr
    mail <- liftIO $ simpleMail to self subject plainBody htmlBody []
    conn <- liftIO $ connectSMTPS' host 465
    (code, bs) <- liftIO $ login conn username password
    when (code /= 235) $ throwError (show bs)
    liftIO $ renderAndSend conn mail
    liftIO $ print mail
    liftIO $ closeSMTP conn

genAddress :: Student -> Address
genAddress s = Address Nothing addr where
    host = "s.unibuc.ro"
    user = intercalate "." $ pack . map toLower <$> [prenume s, nume s]
    addr = Data.Text.concat [user, "@", host]

genPlainContent :: IsString a => DateStudent -> a
genPlainContent d = "Salut! Sunt proful de la laboratorul de Programare funcțională, Info Unibuc, anul II."

genHtmlContent nrLab (s, d) = 
    "<p>Salut, " <> fromString (prenume s) <>
    "! Sunt proful de la laboratorul de Programare funcțională, Info Unibuc, anul II, grupa " <> fromString (grupa s) <> ".</p>" <>
    "<p>Am trecut prin " <> fromString (show nrLab) <> "/14 laboratoare. " <>
    "Până astăzi, pe baza evidențelor mele, tu ai obținut <b>" <> fromString (show $ puncteTotal d) <> "p</b>; plus <b>" <>
    fromString (show $ puncteBonus d) <> "p</b> din exercițiile bonus. În total, ai <b>" <> fromString (show $ notaLaborator d) <> "p</b>." <>
    "<p>Maximul pe care îl puteai avea până acum este de " <> fromString (show $ puncteTotal bestResults) <> "p standard și " <>
    fromString (show $ puncteBonus bestResults) <> "p bonus, deci " <> fromString (show $ notaLaborator bestResults) <> "p în total.</p>" <>
    "<ul>" <>
        "<li>Ai fost prezent(ă) la <b>" <> fromString (show $ prezente d) <> "</b> laboratoare;</li>" <>
        "<li>Ai avut activitate la tablă la <b>" <> fromString (show $ iesiri d) <>"</b> dintre ele;</li>" <>
        ceMiaiPrezentat False (ex d) <>
        "<li>Dintre exercițiile bonus:" <>
            "<ul>"  <>
                (if ceMiaiPrezentat True (bonus d) /= "" then ceMiaiPrezentat True (bonus d) else " nimic!") <>
            "</ul>" <>
    "</ul>" <>
    "<p>E posibil să îmi lipsească date din situația ta. În acest caz, te rog să îmi atragi atenția; aici pe mail, sau la facultate.</p>" <>
    "<p>Dacă vrei să recuperezi din laboratoare anterioare, condițiile sunt să le faci <b>singur(ă)</b>, să le poți explica, și să nu prezinți nu mai mult de 3 odată. Și alte condiții de bun-simț se aplică (de ex., nu e ok dacă vrei să recuperezi în ultimele săptămâni un semestru întreg), dar mai discutăm de la caz la caz.</p>" <>
    "<p>Acest mesaj a fost trimis automat. Dacă vrei să vezi cum am calculat punctajul, uită-te <a href=\"https://github.com/alexoltean61/grading-pf-2526/blob/main/app/Formula.hs\">aici</a>. Dacă vrei să afli cum am făcut asta în Haskell, uită-te și prin <a href=\"https://github.com/alexoltean61/grading-pf-2526/tree/main/app\">alte părți</a> ale codului, și întreabă!</p>"
    where bestResults = snd (studentModel nrLab)

ceMiaiPrezentat :: (Semigroup a, IsString a) => Bool -> [(Laborator, Maybe String)] -> a
ceMiaiPrezentat isBonus [] = ""
ceMiaiPrezentat isBonus ((l, Nothing) : t) = "<li>În laboratorul " <> fromString (show l) <> " mi-ai lăsat impresia că ai folosit cam mult LLM-uri. Ca să mă asigur că ai înțeles exercițiile " <> bonusString isBonus <> " și să ți le pot puncta, o să te rog să îl lucrezi de mână și să îl mai prezinți o dată, oricând ești pregătit. Îmi cer scuze dacă greșesc.</li>" <> ceMiaiPrezentat isBonus t
ceMiaiPrezentat isBonus ((l, Just "") : t) = ceMiaiPrezentat isBonus t
ceMiaiPrezentat isBonus ((l, Just str) : t) = "<li>Din laboratorul " <> fromString (show l) <> ", mi-ai prezentat exercițiile " <> bonusString isBonus <> ": " <> fromString str <> ";</li>" <> ceMiaiPrezentat isBonus t

bonusString isBonus
    | isBonus = "<b>bonus</b>"
    | otherwise = ""
