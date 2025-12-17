{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import Control.Monad
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except
import System.Environment (getArgs)
import Model
import Parser
import Formula


main :: IO ()
main = do
    args <- getArgs
    when (length args /= 1) $
        error $ "Wrong number of arguments: expected 1, got " ++ show (length args)
    let filename = head args
    evalExcept (processCsv filename)
        (\v ->
            V.forM_ v $ \ ((student, situatie) :: StudentPair) ->
            print (nume student, prenume student, " normal ", puncteTotal situatie, " bonus ", puncteBonus situatie, " total ", notaLaborator situatie)
        )
        error

-- Helpers --

evalExcept :: (Monad m) => ExceptT e m a -> (a -> m b) -> (e -> m b) -> m b
evalExcept t success callback = do
    res <- runExceptT t
    case res of
        Left err -> callback err
        Right v  -> success v
