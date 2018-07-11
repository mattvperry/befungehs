module Main where

import Interpreter (MonadBefunge(..), runProgram)
import Control.Monad (unless)
import System.Environment (getArgs)
import System.Random (newStdGen)
import System.IO (hFlush, stdout)

instance MonadBefunge IO where
    tellInt  n = (putStr . show $ n) >> hFlush stdout
    tellChar c = putChar c >> hFlush stdout
    askInt     = readLn
    askChar    = head <$> getLine

main :: IO ()
main = do
    args <- getArgs
    unless (length args == 1) (error "supply a file path")
    p <- readFile . head $ args
    g <- newStdGen
    runProgram g p