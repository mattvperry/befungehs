module Interpreter
    ( Interpreter
    , Command(..)
    , runProgram
    )
where

import Instructions (Command(..), step)
import Program (Program, load)
import Control.Applicative (many)
import Control.Monad.Prompt (MonadPrompt(..), Prompt, runPromptM)
import Control.Monad.State (StateT, execStateT, lift)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import System.Random (StdGen)

type Interpreter m a = Command a -> m a
type Machine = Prompt Command
type App = MaybeT (StateT Program Machine)

execApp :: App a -> Program -> Machine Program
execApp = execStateT . runMaybeT

runProgram
    :: Monad m => (forall a . Interpreter m a) -> StdGen -> String -> m ()
runProgram i g s = do
    runPromptM i . execApp (many step) . load g $ s
    return ()

instance (Monad m, MonadPrompt p m) => MonadPrompt p (MaybeT m) where
    prompt = lift . prompt

instance (Monad m, MonadPrompt p m) => MonadPrompt p (StateT s m) where
    prompt = lift . prompt
