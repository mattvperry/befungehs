module Main where

import           Control.Lens
import           App                                      ( runProgram )
import           System.Random                            ( newStdGen )

main :: IO ()
--main = putStrLn . (maybe "" show) . parse $ ">987v>.v\nv456<  :\n>321 ^ _@"
main = do
    g <- newStdGen
    print . runProgram g $ ">987v>.v\nv456<  :\n>321 ^ _@"
