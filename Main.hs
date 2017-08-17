module Main where

import Direction
import Position
import Robot

main :: IO ()
main = do
  let r = Robot (Position 1 3) North

  let filename = "robot-test.in"
  _ <- readFile filename

  Robot.report r
