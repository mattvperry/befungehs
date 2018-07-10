module Instructions
    ( Command(..)
    , step
    )
where

import Program (Program, cursor, dir, stack, gen, field)
import PlayField (Dir(..), move)
import Control.Applicative (many)
import Control.Lens (use, preuse, ix, uncons, (%=), (.=))
import Control.Monad (MonadPlus, mzero)
import Control.Monad.Prompt (MonadPrompt, prompt)
import Control.Monad.State (MonadState)
import Data.Char (isDigit, digitToInt, chr, ord)
import System.Random (random)

type MonadApp m = (MonadPrompt Command m, MonadPlus m, MonadState Program m)

data Command :: * -> * where
    COutNum :: Int -> Command ()
    COutChr :: Char -> Command ()
    CInNum  :: Command Int
    CInChr  :: Command Char

step :: MonadApp m => m ()
step = stepWith exec

stepWith :: MonadApp m => (Char -> m ()) -> m ()
stepWith f = do
    p      <- use cursor
    Just c <- preuse $ field . ix p
    f c >> advance

advance :: MonadApp m => m ()
advance = do
    d <- use dir
    p <- use field
    cursor %= move p d

peek :: MonadApp m => m Int
peek = use stack >>= return . maybe 0 fst . uncons

pop :: MonadApp m => m Int
pop = use stack >>= maybe (return 0) f . uncons
    where f (x, xs) = stack .= xs >> return x

popPair :: MonadApp m => m (Int, Int)
popPair = do
    y <- pop
    x <- pop
    return (x, y)

push :: MonadApp m => Int -> m ()
push x = stack %= (x :)

doOp :: MonadApp m => (Int -> Int -> a) -> (a -> m ()) -> m ()
doOp f g = uncurry f <$> popPair >>= g

exec :: MonadApp m => Char -> m ()
exec c | isDigit c = push . digitToInt $ c
exec '^'           = dir .= U
exec 'v'           = dir .= D
exec '<'           = dir .= L
exec '>'           = dir .= R
exec '#'           = advance
exec '+'           = doOp (+) push
exec '-'           = doOp (-) push
exec '*'           = doOp (*) push
exec '/'           = doOp div push
exec '%'           = doOp mod push
exec ':'           = peek >>= push
exec '$'           = pop >> return ()
exec '@'           = mzero
exec ' '           = return ()
exec '.'           = pop >>= prompt . COutNum
exec ','           = pop >>= prompt . COutChr . chr
exec '&'           = prompt CInNum >>= push
exec '~'           = prompt CInChr >>= push . ord
exec '\\'          = do
    x <- pop
    y <- pop
    push x
    push y
exec '`' = doOp (>) $ \case
    True  -> push 1
    False -> push 0
exec '!' = pop >>= \case
    0 -> push 1
    _ -> push 0
exec '_' = pop >>= \case
    0 -> exec '>'
    _ -> exec '<'
exec '|' = pop >>= \case
    0 -> exec 'v'
    _ -> exec '^'
exec 'p' = do
    p <- popPair
    v <- pop
    field . ix p .= chr v
exec 'g' = do
    p      <- popPair
    Just x <- preuse $ field . ix p
    push . ord $ x
exec '?' = do
    (d, g) <- random <$> use gen
    dir .= d
    gen .= g
exec '"' = advance >> many (stepWith stringMode) >> return ()
  where
    stringMode '"' = mzero
    stringMode c   = push . ord $ c
exec c = error ("invalid operation: " ++ show c)
