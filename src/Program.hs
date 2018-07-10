module Program
    ( Program
    , load
    , cursor
    , dir
    , stack
    , gen
    , field
    )
where

import PlayField (PlayField, Pos, Dir(R), parse)
import Control.Lens (makeLenses)
import System.Random (StdGen)

data Program = Program
    { _cursor   :: Pos
    , _dir      :: Dir
    , _stack    :: [Int]
    , _gen      :: StdGen
    , _field    :: PlayField
    }
    deriving (Show)
makeLenses ''Program

load :: StdGen -> String -> Program
load g = Program (0, 0) R [] g . parse
