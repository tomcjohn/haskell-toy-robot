module Robot where

import Position
import Direction

data Robot = Robot Position Direction

instance Show Robot where
  show (Robot p d) = (show p) ++ "," ++ (show d)

move :: Robot -> Robot
move (Robot p d) = Robot p d

left :: Robot -> Robot
left (Robot p d) = Robot p (turnLeft d)

right :: Robot -> Robot
right (Robot p d) = Robot p (turnRight d)

main :: IO ()
main = do
  let r = Robot (Position 1 1) North
  putStrLn(show(r))
