module Main where

import Direction
import Position
import Robot
import Table

main :: IO ()
main = do
  let r = Robot (Position 3 4) West

  let filename = "robot-test.in"
  content <- readFile filename

  let t = Table (Position 0 0) (Position 5 5)

  let _ = map (doCommand t) (lines content)

  Robot.report r
