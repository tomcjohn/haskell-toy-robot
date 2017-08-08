module Table where

import Data.List.Split

import Direction
import Position
import Robot

data Table = Table Position Position

doCommand :: String -> IO ()
doCommand cmd = do
  let splitCmd = splitOn " " cmd
  let f = handleCommand (splitCmd !! 0)
  let newR = f (Robot (Position 1 1) North)
  putStrLn (show newR)

handleCommand :: String -> Robot -> Robot
handleCommand "PLACE" = doPlace
handleCommand "MOVE" = Robot.move
handleCommand "LEFT" = Robot.left
handleCommand "RIGHT" = Robot.right
handleCommand "REPORT" = doReport
handleCommand _ = id

doPlace :: Robot -> Robot
doPlace = id
doReport :: Robot -> Robot
doReport = id
