module Robot where

import qualified Orientation as O
import           Orientation (Orientation)

import qualified Pos         as P
import           Pos         (Pos)

data Robot = Robot
  { orientation::Orientation
  , position::Pos
  } deriving Show

left :: Robot -> Robot
left r = Robot (O.left $ orientation r) $ position r

right :: Robot -> Robot
right r = Robot (O.right $ orientation r) $ position r

move :: Robot -> Robot
move r = Robot (orientation r) $ P.move (orientation r) (position r)

report :: Robot -> IO Robot
report r = do
  print $ (show $ orientation r) ++ " " ++ (show $ position r)
  pure r
