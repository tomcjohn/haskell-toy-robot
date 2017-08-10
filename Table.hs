module Table where

import Command
import Direction
import Position
import Robot

data Table = Table Position Position

onTable :: Table -> Robot -> Bool
onTable (Table (Position x1 y1) (Position x2 y2)) (Robot (Position rx ry) _) =
  (rx >= x1) && (rx <= x2) && (ry >= y1) && (ry <= y2)

doCommand :: Command -> Table -> IO Robot
doCommand c t = do
  let r = Robot (Position 1 1) North
  handleCmd c t r

handleCmd :: Command -> Table -> Robot -> IO Robot
handleCmd (Place x y d) t r = doPlace x y d t r
handleCmd Move t r = doMove t r
handleCmd Command.Left _ r = Robot.left r
handleCmd Command.Right _ r = Robot.right r
handleCmd Report _ r = Robot.report r
handleCmd Unrecognised _ r = skipCmd r

doPlace :: Int -> Int -> Direction -> Table -> Robot -> IO Robot
doPlace x y d t r = do
  let newR = Robot (Position x y) d
  if (onTable t newR) then pure newR else pure r

doMove :: Table -> Robot -> IO Robot
doMove t r = do
  newR <- Robot.move r
  if (onTable t newR) then pure newR else pure r

skipCmd :: Robot -> IO Robot
skipCmd r = do
  putStrLn("Unrecognised command")
  pure r
