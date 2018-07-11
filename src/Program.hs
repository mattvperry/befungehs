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
import System.Random (RandomGen)

data Program g = Program
    { _cursor   :: Pos
    , _dir      :: Dir
    , _stack    :: [Int]
    , _gen      :: g
    , _field    :: PlayField
    }
    deriving (Show)
makeLenses ''Program

load :: (RandomGen g) => g -> String -> Program g
load g = Program (0, 0) R [] g . parse
