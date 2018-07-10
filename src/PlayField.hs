module PlayField
    ( PlayField
    , Pos
    , Dir(..)
    , parse
    , move
    )
where

import Control.Lens.At (Ixed(..), IxValue(..), Index(..))
import Data.List.Split (chunksOf)
import Data.Vector.Unboxed (Vector, fromList, toList, (//), (!))
import System.Random (Random(..))

type Pos = (Int, Int)

data Dir = U | D | L | R deriving (Eq, Show, Bounded, Enum)

data PlayField = PF
        { width :: Int
        , height :: Int
        , field :: Vector Char
        }
        deriving (Eq)

instance Random Dir where
    randomR (a, b) g = let (r, g') = randomR (fromEnum a, fromEnum b) g in (toEnum r, g')
    random = randomR (minBound, maxBound)

instance Show PlayField where
    show PF {..} = unlines . chunksOf width . toList $ field

type instance Index PlayField = Pos
type instance IxValue PlayField = Char
instance Ixed PlayField where
    ix p f pf@PF {..} = PF width height . (field //) . (:[]) . (i, ) <$> f (field ! i)
        where i = let (x, y) = wrap pf p in y * width + x
    {-# INLINE ix #-}

move :: PlayField -> Dir -> Pos -> Pos
move p U (x, y) = wrap p (x, y - 1)
move p D (x, y) = wrap p (x, y + 1)
move p L (x, y) = wrap p (x - 1, y)
move p R (x, y) = wrap p (x + 1, y)

wrap :: PlayField -> Pos -> Pos
wrap PF {..} (x, y) = (x `mod` width, y `mod` height)

parse :: String -> PlayField
parse s = PF w h . fromList $ concatMap (pad w ' ') ls
  where
    ls = lines s
    h  = length ls
    w  = maximum . map length $ ls

pad :: Int -> a -> [a] -> [a]
pad n x xs
    | s < n     = xs ++ replicate (n - s) x
    | otherwise = xs
    where s = length xs