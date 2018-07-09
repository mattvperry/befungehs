{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Instructions
    ( step
    )
where

import           Program                                  ( Program
                                                          , cursor
                                                          , dir
                                                          , stack
                                                          , gen
                                                          , field
                                                          , out
                                                          )
import           PlayField                                ( Dir(..)
                                                          , move
                                                          )
import           Control.Lens                             ( use
                                                          , preuse
                                                          , ix
                                                          , uncons
                                                          , (%=)
                                                          , (.=)
                                                          , (<|)
                                                          , (<>=)
                                                          )
import           Control.Monad                            ( MonadPlus
                                                          , mzero
                                                          )
import           Control.Monad.State                      ( MonadState )
import           Data.Char                                ( isDigit
                                                          , digitToInt
                                                          , chr
                                                          , ord
                                                          )
import           System.Random                            ( random )

type MonadApp m = (MonadPlus m, MonadState Program m)

step :: MonadApp m => m ()
step = do
    p      <- use cursor
    Just c <- preuse $ field . ix p
    exec c >> advance

advance :: MonadApp m => m ()
advance = use dir >>= (cursor %=) . move

peek :: MonadApp m => m Int
peek = use stack >>= return . maybe 0 fst . uncons

push :: MonadApp m => Int -> m ()
push x = stack %= (x <|)

pop :: MonadApp m => m Int
pop = uncons <$> use stack >>= \case
    Nothing      -> return 0
    Just (x, xs) -> stack .= xs >> return x

popPair :: MonadApp m => m (Int, Int)
popPair = do
    y <- pop
    x <- pop
    return (x, y)

doOp :: MonadApp m => (Int -> Int -> a) -> (a -> m ()) -> m ()
doOp f g = uncurry f <$> popPair >>= g

exec :: MonadApp m => Char -> m ()
exec c | isDigit c = push . digitToInt $ c
exec '>'           = dir .= E
exec '<'           = dir .= W
exec '^'           = dir .= N
exec 'v'           = dir .= S
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
exec '"' = _
exec '.' = pop >>= (out <>=) . show
exec ',' = pop >>= (out <>=) . (:[]) . chr
exec c = error ("invalid operation: " ++ show c)
