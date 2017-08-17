module Command where

import Data.List.Split

import Direction
import Position

data Command = Place Position Direction | Move | Left | Right | Report | Unrecognised

lookup :: String -> String -> Command
lookup "PLACE" s = do
  let splitCmd = splitOn "," s
  let newX = read (splitCmd!!0)
  let newY = read (splitCmd!!1)
  let newDir = Direction.lookup (splitCmd!!2)
  Place (Position newX newY) newDir
lookup "MOVE" _ = Move
lookup "LEFT" _ = Command.Left
lookup "RIGHT" _ = Command.Right
lookup "REPORT" _ = Report
lookup _ _ = Unrecognised
