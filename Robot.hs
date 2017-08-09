module Robot where

import Position
import Direction
import Direction (move, left, right)

data Robot = Robot Position Direction

instance Show Robot where
  show (Robot p d) = (show p) ++ "," ++ (show d)

move :: Robot -> IO Robot
move (Robot p d) = return (Robot (Direction.move d p) d)

left :: Robot -> IO Robot
left (Robot p d) = return (Robot p (Direction.left d))

right :: Robot -> IO Robot
right (Robot p d) = return (Robot p (Direction.right d))

report :: Robot -> IO Robot
report r = do
  putStrLn(show r)
  return r
