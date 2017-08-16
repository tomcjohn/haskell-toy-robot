module Table where

import Control.Monad.IO.Class
import Control.Monad.Trans.State

import Command
import Position
import Robot

data Table = Table Position Position
type GameState = StateT (Maybe Robot) IO ()

onTable :: Table -> Maybe Robot -> Bool
onTable t (Just r) = robotOnTable t r
onTable _ Nothing = False

robotOnTable :: Table -> Robot -> Bool
robotOnTable (Table (Position x1 y1) (Position x2 y2)) (Robot (Position rx ry) _) =
  (rx >= x1) && (rx <= x2) && (ry >= y1) && (ry <= y2)

doCommand :: String -> Table -> GameState
doCommand s t = handleCmd (toCommand s) t

handleCmd :: Command -> Table -> GameState
handleCmd (Place x y d) t = do
  let newR = Robot (Position x y) d
  if (robotOnTable t newR) then put (Just newR) else pure ()
handleCmd Move t = adjustWithCheck Robot.move (onTable t)
handleCmd Command.Left _ = adjust Robot.left
handleCmd Command.Right _ = adjust Robot.right
handleCmd Report _ = do
  maybeRobot <- get
  doReport maybeRobot
handleCmd Unrecognised _ = pure ()

doReport :: Maybe Robot -> GameState
doReport (Just r) = do
  liftIO (Robot.report r)
  pure ()
doReport Nothing = pure ()

adjust :: (Robot -> Robot) -> GameState
adjust action = adjustWithCheck action always

adjustWithCheck :: (Robot -> Robot) -> (Maybe Robot -> Bool) -> GameState
adjustWithCheck action accept = do
  maybeRobot <- get
  let newRobot = fmap action maybeRobot :: Maybe Robot
  if (accept newRobot) then (put newRobot) else pure ()

always :: a -> Bool
always _ = True
