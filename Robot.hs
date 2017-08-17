module Robot where

import Direction
import Position

data Robot = Robot Position Direction

instance Show Robot where
  show (Robot p d) = "(" ++ show p ++ "," ++ show d ++ ")"

move :: Robot -> Robot
move (Robot p d) = Robot (Direction.move d p) d

left :: Robot -> Robot
left (Robot p d) = Robot p (Direction.left d)

right :: Robot -> Robot
right (Robot p d) = Robot p (Direction.right d)

report :: Robot -> IO ()
report r = print r
