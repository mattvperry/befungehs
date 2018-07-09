{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}

module PlayField
    ( PlayField
    , Pos
    , Dir(..)
    , parse
    , move
    )
where

import           Control.Lens.At                          ( Ixed(..)
                                                          , IxValue(..)
                                                          , Index(..)
                                                          )
import           Data.List.Split                          ( chunksOf )
import qualified Data.Vector.Unboxed           as V
import           System.Random                            ( Random(..) )

type Pos = (Int, Int)

data Dir = N | S | E | W deriving (Eq, Show, Bounded, Enum)

newtype PlayField = PF { getField :: V.Vector Char }
        deriving (Eq)

width :: Int
width = 80

height :: Int
height = 25

move :: Dir -> Pos -> Pos
move N (x, y) = wrap (x, y - 1)
move S (x, y) = wrap (x, y + 1)
move E (x, y) = wrap (x + 1, y)
move W (x, y) = wrap (x - 1, y)

parse :: String -> Maybe PlayField
parse s | length l > height = Nothing
        | any ((> width) . length) l = Nothing
        | otherwise = Just . PF . V.fromList $ concatMap (pad width ' ') l
    where l = pad height (replicate width ' ') . lines $ s

pad :: Int -> a -> [a] -> [a]
pad n x xs | s < n     = xs ++ replicate (n - s) x
           | otherwise = xs
    where s = length xs

wrap :: Pos -> Pos
wrap (x, y) = (x `mod` width, y `mod` height)

instance Random Dir where
    randomR (a, b) g = let (r, g') = randomR (fromEnum a, fromEnum b) g in (toEnum r, g')
    random = randomR (minBound, maxBound)

instance Show PlayField where
    show = unlines . chunksOf width . V.toList . getField

-- Lens compat
type instance Index PlayField = Pos
type instance IxValue PlayField = Char
instance Ixed PlayField where
    ix p f pf = PF . (v V.//) . (:[]) . (i, ) <$> f (v V.! i)
        where v = getField pf
              i = let (x, y) = wrap p in y * width + x
    {-# INLINE ix #-}
