{-# LANGUAGE RankNTypes #-}

module App
    ( runProgram
    )
where

import           Program                                  ( Program
                                                          , load
                                                          , out
                                                          )
import           Instructions                             ( step )
import           Control.Applicative                      ( many )
import           Control.Lens                             ( (^.) )
import           Control.Monad.State                      ( State
                                                          , execState
                                                          )
import           Control.Monad.Trans.Maybe                ( MaybeT
                                                          , runMaybeT
                                                          )
import           System.Random                            ( StdGen )

type App = MaybeT (State Program)

execApp :: App a -> Program -> Program
execApp = execState . runMaybeT

runProgram :: StdGen -> String -> Program
runProgram g s = case load g s of
    Just p  -> execApp (many step) p
    Nothing -> error "Failed to parse program"
