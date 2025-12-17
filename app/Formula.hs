{-
    În acest modul sunt definite formulele de calcul pentru punctajul vostru.
-}

module Formula where

import Model

-- | Pe fiecare lab rezolvat complet (fără bonus) primiți 0.1p.
--   În fiecare laborator, exercițiile individuale au aceeași pondere,
--   i.e. 1 / (nr. ex).
--   În total pe exercițiile din toate laburile puteți obține maxim 1.3p.
puncteExercitii :: DateStudent -> Double
puncteExercitii s = rotunjeste025 (0.1 * sum punctajExPerLab) where
    punctajExPerLab = snd <$> punctajEx s

-- | Self-explanatory. Primiți 0.1 pe fiecare ieșire la tablă.
--   În total din ieșiri puteți obține maxim 0.6p.
puncteIesiri :: DateStudent -> Double
puncteIesiri s = min 0.6 $ 0.1 * fromIntegral (iesiri s)

-- | Self-explanatory.
--   În total pe baza simplei prezențe puteți obține maxim 0.2p.
punctePrezente :: DateStudent -> Double
punctePrezente s
    | prezente s >= 10 = 0.2
    | prezente s >= 5  = 0.1
    | otherwise = 0

-- | Punctajul total FĂRĂ puncte bonus.
--   În total, fără bonus, puteți obține maxim 2p.
puncteTotal :: DateStudent -> Double
puncteTotal s = min 2 $ rotunjeste01 $ punctePrezente s + puncteIesiri s + puncteExercitii s

-- | Pe fiecare secțiune bonus rezolvată complet primiți 0.2p.
--   În secțiunea bonus, exercițiile / subpunctele au aceeași pondere,
--   i.e. 1 / (nr. ex).
--   În total, din bonus puteți obține maxim 1p.
puncteBonus :: DateStudent -> Double
puncteBonus s = min 1 $ rotunjeste01 (0.2 * sum punctajBonusPerLab) where
    punctajBonusPerLab = snd <$> punctajBonus s

-- | Nota la laborator este suma dintre punctele pe exerciții + activitate + prezență (2p)
--   și bonus (1p).
--   În total: maxim 3p.
notaLaborator :: DateStudent -> Double
notaLaborator s = rotunjeste01 $ puncteTotal s + puncteBonus s

-- | Rotunjește la cel mai apropiat multiplu de (1 / n)
rotunjeste :: Double -> Double -> Double
rotunjeste n x = fromIntegral (round (x * n)) / n

-- | Rotunjește la cel mai apropiat multiplu de 0.25
rotunjeste025 = rotunjeste 4
-- | Rotunjește la cel mai apropiat multiplu de 0.1
rotunjeste01  = rotunjeste 10
