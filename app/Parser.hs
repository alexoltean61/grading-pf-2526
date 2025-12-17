{-
    Acest modul se ocupă de parsarea .csv-ului meu cu datele voastre.
    Puteți ignora.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Parser where

import Model
import Data.List ( intercalate, isInfixOf )
import Data.Csv ( Parser, FromNamedRecord(..), (.:), decodeByName )
import Data.Map ( Map )
import qualified Data.Vector as V
import Control.Monad
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except
import qualified Data.ByteString.Lazy as BL

initStudent :: String -> String -> Student
initStudent numeStr = Student nume (intercalate "-" prenume) where
    (nume:prenume) = words numeStr

initStudentPair :: Student -> StudentPair
initStudentPair s = (s, DateStudent 0 0 [] [] [] [])

addPresence :: String -> StudentPair -> StudentPair
addPresence "P" (s, r) = (s, r { prezente = 1 + prezente r })
addPresence "p" (s, r) = (s, r { prezente = 1 + prezente r })
addPresence _ p   = p

addIesire :: String -> StudentPair -> StudentPair
addIesire "iesit" (s, r) = (s, r { iesiri = 1 + iesiri r })
addIesire "Iesit" (s, r) = (s, r { iesiri = 1 + iesiri r })
addIesire _ p   = p

addLab :: Laborator -> Integer -> String -> StudentPair -> StudentPair
addLab lab total exStr (s, r)
    | "(LLM)" `isInfixOf` exStr  = (s, r { ex = (lab, Nothing) : ex r, punctajEx = (lab, 0) : punctajEx r })
    | otherwise  = (s, r { ex = (lab, Just exStr) : ex r, punctajEx = (lab, procent) : punctajEx r })
    where
        nrRezolvate = (toInteger . length) (read $ concat ["[", exStr, "]"] :: [Integer])
        procent = fromInteger nrRezolvate / fromInteger total

addBonus :: Laborator -> Integer -> String -> StudentPair -> StudentPair
addBonus lab total exStr (s, r)
    | "(LLM)" `isInfixOf` exStr  = (s, r { bonus = (lab, Nothing) : bonus r, punctajBonus = (lab, 0) : punctajBonus r })
    | otherwise  = (s, r { bonus = (lab, Just exStr) : bonus r, punctajBonus = (lab, procent) : punctajBonus r })
    where
        nrRezolvate = (toInteger . length) (read $ concat ["[", exStr, "]"] :: [Integer])
        procent = fromInteger nrRezolvate / fromInteger total

instance FromNamedRecord StudentPair where
    parseNamedRecord r =
        populateStudentPair $ initStudentPair <$> (initStudent <$> r .: "NUME" <*> r .: "GRUPA")
        where
            l1P = r .: "L1 P (09-10.10.25)" :: Parser String
            l2P = r .: "L2 P (16-17.10.25)" :: Parser String
            l3P = r .: "L3 P (23-24.10.25)" :: Parser String
            l4P = r .: "L4 P (30-31.10.25)" :: Parser String
            l5P = r .: "L5 P (06-07.11.25)" :: Parser String
            l6P = r .: "L6 P (13-14.11.25)" :: Parser String
            l7P = r .: "L7 P (20-21.11.25 racit)" :: Parser String
            l8P = r .: "L8 P (27-28.11.25)" :: Parser String
            l9P = r .: "L9 P (04-05.12.25)" :: Parser String
            l10P = r .: "L10 P (11-12.12.25)" :: Parser String
            l2I = r .: "L2 iesit (16-17.10.25)" :: Parser String
            l3I = r .: "L3 iesit (23-24.10.25)" :: Parser String
            l4I = r .: "L4 iesit (30-31.10.25)" :: Parser String
            l5I = r .: "L5 iesit (06-07.11.25)" :: Parser String
            l6I = r .: "L6 iesit (13-14.11.25)" :: Parser String
            l7I = r .: "L7 iesit (20-21.11.25 racit)" :: Parser String
            l8I = r .: "L8 iesit (27-28.11.25)" :: Parser String
            l9I = r .: "L9 iesit (04-05.12.25)" :: Parser String
            l10I = r .: "L10 iesit (11-12.12.25)" :: Parser String
            l2Ex = ((2,11),) <$> (r .: "L2 Ex (16-17.10.25) /11" :: Parser String)
            l3Ex = ((3,10),) <$> (r .: "L3 Ex (23-24.10.25) /10" :: Parser String)
            l4Ex = ((4,13),) <$> (r .: "L4 Ex (30-31.10.25) /13" :: Parser String)
            l5Ex = ((5,13),) <$> (r .: "L5 Ex (06-07.11.25) /13" :: Parser String)
            l6Ex = ((6,3),)  <$> (r .: "L6 Ex (13-14.11.25) /3" :: Parser String)
            l7Ex = ((7,11),) <$> (r .: "L7 Ex (20-11.11.25 racit) /11" :: Parser String)
            l8Ex = ((8,7),)  <$> (r .: "L8 Ex (27-28.11.25) /7" :: Parser String)
            l9Ex = ((9,3),)  <$> (r .: "L9 Ex (04-05.12.25) /3" :: Parser String)
            l10Ex = ((10,9),) <$> (r .: "L10 Ex (11-12.12.25) /9" :: Parser String)
            l2B = ((2,1),) <$> (r .: "L2 bonus (16-17.10.25) /12" :: Parser String)
            l3B = ((3,4),) <$> (r .: "L3 bonus (23-24.10.25) / 11-14" :: Parser String)
            l4B = ((4,5),) <$> (r .: "L4 bonus (30-31.10.25) / 14-18" :: Parser String)
            l5B = ((5,4),) <$> (r .: "L5 bonus (06-07.11.25) / 14" :: Parser String)
            l6B = ((6,6),) <$> (r .: "L6 bonus (13-14.11.25) / 4-9" :: Parser String)
            l7B = ((7,1),) <$> (r .: "L7 bonus (20-21.11.25 racit) / 12" :: Parser String)
            l8B = ((8,1),) <$> (return "" :: Parser String)
            l9B = ((9,7),) <$> (r .: "L9 bonus (04-05.12.25) / 7" :: Parser String)
            l10B = ((10,1),) <$> (r .: "L10 bonus (04-05.12.25) / 1" :: Parser String)
            parserePrezente = [l1P, l2P, l3P, l4P, l5P, l6P, l7P, l8P, l9P, l10P]
            parsereIesiri = [l2I, l3I, l4I, l5I, l6I, l7I, l8I, l9I, l10I]
            parsereLabs = [l2Ex, l3Ex, l4Ex, l5Ex, l6Ex, l7Ex, l8Ex, l9Ex, l10Ex]
            parsereBonus = [l2B, l3B, l4B, l5B, l6B, l7B, l8B, l9B, l10B]

            sumParse f l = foldl (.) id $ (<*>) <$> (f <$>) <$> l
            totalPrezente = sumParse addPresence parserePrezente
            totalIesiri = sumParse addIesire parsereIesiri
            totalEx = sumParse (uncurry $ uncurry addLab) parsereLabs
            totalBonus = sumParse (uncurry $ uncurry addBonus) parsereBonus
            
            populateStudentPair = totalBonus . totalEx . totalIesiri . totalPrezente

processCsv :: String -> ExceptT String IO (V.Vector StudentPair)
processCsv filename = do
    csvData <- lift $ BL.readFile filename
    (_, v)  <- except $ decodeByName csvData
    return v
