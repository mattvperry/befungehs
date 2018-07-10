module Main where

import Interpreter (Interpreter, Command(..), runProgram)
import Control.Monad (unless)
import System.Environment (getArgs)
import System.Random (newStdGen)

interpret :: Interpreter IO a
interpret (COutNum n) = putStr . show $ n
interpret (COutChr c) = putChar c
interpret CInNum      = readLn :: IO Int
interpret CInChr      = getChar

main :: IO ()
main = do
    args <- getArgs
    unless (length args == 1) (error "supply a file path")
    p <- readFile . head $ args
    g <- newStdGen
    runProgram interpret g p