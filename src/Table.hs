module Table where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.List.Split (splitOn)

import qualified Command         as C
import           Command         (Command (..))
import           Position        (Position)
import qualified Robot           as R
import           Robot           (Robot (..))

data Table = Table Position Position

type GameState = StateT (Maybe Robot) IO ()

onTable :: Table -> Maybe Robot -> Bool
onTable t (Just r) = checkOnTable t r
onTable _ _        = False

checkOnTable :: Table -> Robot -> Bool
checkOnTable (Table (x1,y1) (x2,y2)) (Robot (x,y) _) =
  x >= x1 && x <= x2 && y >= y1 && y <= y2

doCommand :: Table -> String -> GameState
doCommand t s = handleCmd t (parseCommand s)

parseCommand :: String -> Command
parseCommand str = do
  let splitCmd = splitOn " " str
  C.lookup (head splitCmd) (splitCmd!!1)

handleCmd :: Table -> Command -> GameState
handleCmd t (Place p d)    = do
  let newRobot = Just $ Robot p d
  if onTable t newRobot
    then put newRobot
    else pure()
handleCmd t C.Move         = affectRobotWithCheck R.move (onTable t)
handleCmd _ C.Left         = affectRobot R.left
handleCmd _ C.Right        = affectRobot R.right
handleCmd _ C.Report       = do
  maybeRobot <- get
  doReport maybeRobot
handleCmd _ C.Unrecognised = pure ()

doReport :: Maybe Robot -> GameState
doReport (Just r) = liftIO $ R.report r
doReport Nothing  = pure ()

affectRobot :: (Robot -> Robot) -> GameState
affectRobot action = affectRobotWithCheck action always

affectRobotWithCheck :: (Robot -> Robot) -> (Maybe Robot -> Bool) -> GameState
affectRobotWithCheck action accept = do
  maybeRobot <- get
  let newRobot = action <$> maybeRobot
  if accept newRobot then put newRobot else pure ()

always :: Maybe Robot -> Bool
always _ = True
