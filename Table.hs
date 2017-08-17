module Table where

import Control.Monad.Trans.State
import Data.List.Split

import Command
import Position
import Robot

data Table = Table Position Position

type GameState = StateT (Maybe Robot) IO ()

onTable :: Table -> Maybe Robot -> Bool
onTable t (Just r) = robotOnTable t r
onTable _ Nothing = False

robotOnTable :: Table -> Robot -> Bool
robotOnTable (Table (Position x1 y1) (Position x2 y2)) (Robot (Position rx ry ) _) =
  rx >= x1 && rx <= x2 && ry >= y1 && ry <= y2

doCommand :: Table -> String -> GameState
doCommand t s = do
  let splitCmd = splitOn " " s
  let c = Command.lookup (splitCmd!!0) (splitCmd!!1)
  handleCmd c t

handleCmd :: Command -> Table -> GameState
handleCmd (Place p d) t = do
  let newR = Robot p d
  if (robotOnTable t newR) then put (Just newR) else pure ()
handleCmd Move t = adjustRobot (Robot.move) (onTable t)
handleCmd Command.Left _ = alwaysAdjust Robot.left
handleCmd Command.Right _ = alwaysAdjust Robot.right
handleCmd Report _ = pure ()
handleCmd Unrecognised _ = pure ()

alwaysAdjust :: (Robot -> Robot) -> GameState
alwaysAdjust action = adjustRobot action always

always :: Maybe Robot -> Bool
always _ = True

adjustRobot :: (Robot -> Robot) -> (Maybe Robot -> Bool) -> GameState
adjustRobot action accept = do
  maybeRobot <- get
  let newR = fmap action maybeRobot
  if (accept newR) then put newR else pure ()
