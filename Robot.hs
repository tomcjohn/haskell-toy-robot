module Robot where

import Position
import Direction
import Direction (move, left, right)

data Robot = Robot Position Direction

instance Show Robot where
  show (Robot p d) = (show p) ++ "," ++ (show d)

move :: Robot -> IO Robot
move (Robot p d) = pure (Robot (Direction.move d p) d)

left :: Robot -> IO Robot
left (Robot p d) = pure (Robot p (Direction.left d))

right :: Robot -> IO Robot
right (Robot p d) = pure (Robot p (Direction.right d))

report :: Robot -> IO Robot
report r = do
  putStrLn(show r)
  pure r
