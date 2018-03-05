module Robot where

import qualified Direction  as O
import           Direction  (Direction)

import qualified Position   as P
import           Position   (Position)

data Robot = Robot
  { direction::Direction
  , position::Position
  } deriving Show

left :: Robot -> Robot
left r = Robot (O.left $ direction r) $ position r

right :: Robot -> Robot
right r = Robot (O.right $ direction r) $ position r

move :: Robot -> Robot
move r = Robot (direction r) $ P.move (direction r) (position r)

report :: Robot -> IO Robot
report r = do
  print $ (show $ direction r) ++ " " ++ (show $ position r)
  pure r
