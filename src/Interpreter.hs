module Interpreter
    ( MonadBefunge(..)
    , runProgram
    )
where

import Instructions (MonadBefunge(..), step)
import Program (Program, load)
import Control.Applicative (many)
import Control.Monad.State (StateT, execStateT, lift)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import System.Random (StdGen)

type App m = MaybeT (StateT Program m)

execApp :: (Monad m, MonadBefunge m) => App m a -> Program -> m Program
execApp = execStateT . runMaybeT

runProgram :: (Monad m, MonadBefunge m) => StdGen -> String -> m ()
runProgram g s = do
    execApp (many step) . load g $ s
    return ()

instance (Monad m, MonadBefunge m) => MonadBefunge (StateT s m) where
    tellChar = lift . tellChar
    tellInt  = lift . tellInt
    askChar  = lift askChar
    askInt   = lift askInt

instance (Monad m, MonadBefunge m) => MonadBefunge (MaybeT m) where
    tellChar = lift . tellChar
    tellInt  = lift . tellInt
    askChar  = lift askChar
    askInt   = lift askInt