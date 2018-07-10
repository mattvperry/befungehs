module Main where

import Interpreter (Interpreter, Command(..), runProgram)
import Control.Monad (unless)
import System.Environment (getArgs)
import System.Random (newStdGen)
import System.IO (hFlush, stdout)

interpret :: Interpreter IO a
interpret (COutNum n) = (putStr . show $ n) >> hFlush stdout
interpret (COutChr c) = putChar c >> hFlush stdout
interpret CInNum      = readLn :: IO Int
interpret CInChr      = getChar

main :: IO ()
main = do
    args <- getArgs
    unless (length args == 1) (error "supply a file path")
    p <- readFile . head $ args
    g <- newStdGen
    runProgram interpret g p