module Table where

import Data.List.Split

import Direction
import Position
import Robot

data Table = Table Position Position

doCommand :: String -> Table -> IO Robot
doCommand cmd t = do
  let splitCmd = splitOn " " cmd
  let f = handleCommand (splitCmd!!0) (splitCmd!!1) t
  f (Robot (Position 1 1) North)

handleCommand :: String -> String -> Table -> Robot -> IO Robot
handleCommand "PLACE" placeCmd = doPlace placeCmd
handleCommand "MOVE" _ = doMove
handleCommand "LEFT" _ = doLeft
handleCommand "RIGHT" _ = doRight
handleCommand "REPORT" _ = doReport
handleCommand _ _ = skipCmd

doPlace :: String -> Table -> Robot -> IO Robot
doPlace cmd t r = do
  let splitCmd = splitOn "," cmd
  let newX = read (splitCmd!!0)
  let newY = read (splitCmd!!1)
  let newDir = Direction.lookup (splitCmd!!2)
  let newR = Robot (Position newX newY) newDir
  if (onTable t newR) then return newR else return r

doMove :: Table -> Robot -> IO Robot
doMove t r = do
  newR <- Robot.move r
  if (onTable t newR) then return newR else return r

doLeft :: Table -> Robot -> IO Robot
doLeft _ = Robot.left

doRight :: Table -> Robot -> IO Robot
doRight _ = Robot.right

doReport :: Table -> Robot -> IO Robot
doReport _ = Robot.report

skipCmd :: Table -> Robot -> IO Robot
skipCmd _ r = return r

onTable :: Table -> Robot -> Bool
onTable (Table (Position x1 y1) (Position x2 y2)) (Robot (Position rx ry) _) =
  (rx >= x1) && (rx <= x2) && (ry >= y1) && (ry <= y2)
