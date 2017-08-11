module Robot where

import Position
import Direction
import Direction (move, left, right)

data Robot = Robot Position Direction

instance Show Robot where
  show (Robot p d) = (show p) ++ "," ++ (show d)

move :: Robot -> Robot
move (Robot p d) = Robot (Direction.move d p) d

left :: Robot -> Robot
left (Robot p d) = Robot p (Direction.left d)

right :: Robot -> Robot
right (Robot p d) = Robot p (Direction.right d)

report :: Robot -> IO Robot
report r = do
  print r
  pure r
