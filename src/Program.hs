{-# LANGUAGE TemplateHaskell #-}

module Program
    ( Program
    , load
    , cursor
    , dir
    , stack
    , gen
    , out
    , field
    )
where

import           PlayField                                ( PlayField
                                                          , Pos
                                                          , Dir(E)
                                                          , parse
                                                          )
import           Control.Lens                             ( makeLenses )
import           System.Random                            ( StdGen )

data Program = Program
    { _cursor :: Pos
    , _dir :: Dir
    , _stack :: [Int]
    , _gen :: StdGen
    , _out :: String
    , _field :: PlayField
    }
    deriving (Show)
makeLenses ''Program

load :: StdGen -> String -> Maybe Program
load g = fmap (Program (0, 0) E [] g "") . parse
