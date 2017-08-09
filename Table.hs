module Table where

import Data.List.Split

import Direction
import Position
import Robot

data Table = Table Position Position

doCommand :: Table -> String -> IO Robot
doCommand t cmd = do
  let splitCmd = splitOn " " cmd
  let f = handleCommand (splitCmd!!0) t
  f (Robot (Position 1 1) North)

handleCommand :: String -> Table -> Robot -> IO Robot
handleCommand "PLACE" = doPlace
handleCommand "MOVE" = doMove
handleCommand "LEFT" = doLeft
handleCommand "RIGHT" = doRight
handleCommand "REPORT" = doReport
handleCommand _ = skipCmd

doPlace :: Table -> Robot -> IO Robot
doPlace t r = do
  let placeCmd = "3,4,WEST"
  let splitPlaceCmd = splitOn "," placeCmd
  let newX = read (splitPlaceCmd!!0)
  let newY = read (splitPlaceCmd!!1)
  let newDir = Direction.lookup (splitPlaceCmd!!2)
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
