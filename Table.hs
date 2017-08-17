module Table where

import Data.List.Split

import Command
import Direction
import Position
import Robot

data Table = Table Position Position

onTable :: Table -> Robot -> Bool
onTable (Table (Position x1 y1) (Position x2 y2)) (Robot (Position rx ry ) _) =
  rx >= x1 && rx <= x2 && ry >= y1 && ry <= y2

doCommand :: Table -> String -> Robot
doCommand t s = do
  let splitCmd = splitOn " " s
  let c = Command.lookup (splitCmd!!0) (splitCmd!!1)
  let r = Robot (Position 2 5) East
  handleCmd c t r

handleCmd :: Command -> Table -> Robot -> Robot
handleCmd (Place p d) t r = do
  let newR = Robot p d
  if (onTable t newR) then newR else r
handleCmd Move t r = do
  let newR = Robot.move r
  if (onTable t newR) then newR else r
handleCmd Command.Left _ r = Robot.left r
handleCmd Command.Right _ r = Robot.right r
handleCmd Report _ _ = undefined
handleCmd Unrecognised _ _ = undefined
