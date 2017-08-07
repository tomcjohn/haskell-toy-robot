module Robot where

import Position
import Direction

data Robot = Robot (Position, Direction)

main :: IO ()
main = do
  putStrLn("hello world")
