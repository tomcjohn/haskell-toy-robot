module Robot where

import qualified Direction  as O
import           Direction  (Direction)

import qualified Position   as P
import           Position   (Position)

data Robot = Robot
  { position::Position
  , direction::Direction
  } deriving Show

left :: Robot -> Robot
left r = Robot (position r) (O.left $ direction r)

right :: Robot -> Robot
right r = Robot (position r) (O.right $ direction r)

move :: Robot -> Robot
move r = Robot (P.move (direction r) (position r)) (direction r)

report :: Robot -> IO ()
report = print
