{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import Control.Monad
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Except
import System.Environment (getArgs)
import Model
import Parser
import Formula
import Mailer

processAndSend :: String -> Laborator -> ExceptT String IO ()
processAndSend filename nrLab = do
    v <- processCsv filename
    V.forM_ v (\ v -> liftIO (print $ (nume . fst) v) >> sendToStudent nrLab v)

main :: IO ()
main = do
    args <- getArgs
    when (length args /= 2) $
        error $ "Wrong number of arguments: expected 2, got " ++ show (length args)
    let filename = head args
    let nrLab = read (head $ tail args) :: Laborator
    evalExcept (processAndSend filename nrLab) return error

-- Helpers --

evalExcept :: (Monad m) => ExceptT e m a -> (a -> m b) -> (e -> m b) -> m b
evalExcept t success callback = do
    res <- runExceptT t
    case res of
        Left err -> callback err
        Right v  -> success v
