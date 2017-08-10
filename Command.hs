module Command where

import Data.List.Split

import Direction

data Command = Place Int Int Direction | Move | Left | Right | Report | Unrecognised

toCommand :: String -> Command
toCommand s = do
  let splitCmd = splitOn " " s
  Command.lookup (splitCmd!!0) (splitCmd!!1)

lookup :: String -> String -> Command
lookup "PLACE" s = do
  let splitCmd = splitOn "," s
  let x = read (splitCmd!!0)
  let y = read (splitCmd!!1)
  let d = Direction.lookup (splitCmd!!2)
  Place x y d
lookup "MOVE" _ = Move
lookup "LEFT" _ = Command.Left
lookup "RIGHT" _ = Command.Right
lookup "REPORT" _ = Command.Report
lookup _ _ = Unrecognised
