module Main where

import Interpreter (runProgram)
import Control.Monad (unless)
import System.Environment (getArgs)
import System.Random (newStdGen)

main :: IO ()
main = do
    args <- getArgs
    unless (length args == 1) (error "supply a file path")
    p <- readFile . head $ args
    g <- newStdGen
    runProgram g p