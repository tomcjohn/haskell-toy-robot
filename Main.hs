module Main where

import Position
import Robot
import Table

main :: IO ()
main = do
  let filename = "robot-test.in"
  content <- readFile filename

  let t = Table (Position 0 0) (Position 5 5)

  let robots = map (doCommand t) (lines content)

  Robot.report (robots!!10)
