module Table where

import Data.List.Split

import Command
import Direction
import Position
import Robot

data Table = Table Position Position

doCommand :: Table -> String -> Robot
doCommand t s = do
  let splitCmd = splitOn " " s
  let c = Command.lookup (splitCmd!!0) (splitCmd!!1)
  let r = Robot (Position 2 5) East
  handleCmd c t r

handleCmd :: Command -> Table -> Robot -> Robot
handleCmd (Place p d) t r = Robot p d
handleCmd Move t r = Robot.move r
handleCmd Command.Left t r = Robot.left r
handleCmd Command.Right t r = Robot.right r
handleCmd Report _ r = r
handleCmd Unrecognised _ _ = undefined
