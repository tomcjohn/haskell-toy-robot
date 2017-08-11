module Table where

import Control.Monad.State

import Command
import Direction
import Position
import Robot

data Table = Table Position Position
type GameState = State Robot ()

onTable :: Table -> Robot -> Bool
onTable (Table (Position x1 y1) (Position x2 y2)) (Robot (Position rx ry) _) =
  (rx >= x1) && (rx <= x2) && (ry >= y1) && (ry <= y2)

doCommand :: String -> Table -> GameState
doCommand s t = do
  let c = toCommand s
  let r = Robot (Position 1 1) North
  handleCmd c t r

handleCmd :: Command -> Table -> Robot -> GameState
handleCmd (Place x y d) t _ = do
  let newR = Robot (Position x y) d
  if (onTable t newR) then put newR else pure ()
handleCmd Move t r = put (doMove t r)
handleCmd Command.Left _ r = put (Robot.left r)
handleCmd Command.Right _ r = put (Robot.right r)
handleCmd Report _ _ = pure ()
handleCmd Unrecognised _ _ = pure ()

doPlace :: Int -> Int -> Direction -> Table -> Robot -> Robot
doPlace x y d t r = do
  let newR = Robot (Position x y) d
  if (onTable t newR) then newR else r

doMove :: Table -> Robot -> Robot
doMove t r = do
  let newR = Robot.move r
  if (onTable t newR) then newR else r
