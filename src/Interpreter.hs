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
import System.IO (hFlush, stdout)
import System.Random (RandomGen)

type App g m = MaybeT (StateT (Program g) m)

execApp :: (RandomGen g, MonadBefunge m) => App g m a -> Program g -> m (Program g)
execApp = execStateT . runMaybeT

runProgram :: (RandomGen g, MonadBefunge m) => g -> String -> m ()
runProgram g s = do
    execApp (many step) . load g $ s
    return ()

instance MonadBefunge IO where
    tellInt  n = (putStr . show $ n) >> hFlush stdout
    tellChar c = putChar c >> hFlush stdout
    askInt     = readLn
    askChar    = head <$> getLine

instance MonadBefunge m => MonadBefunge (StateT s m) where
    tellChar = lift . tellChar
    tellInt  = lift . tellInt
    askChar  = lift askChar
    askInt   = lift askInt

instance MonadBefunge m => MonadBefunge (MaybeT m) where
    tellChar = lift . tellChar
    tellInt  = lift . tellInt
    askChar  = lift askChar
    askInt   = lift askInt