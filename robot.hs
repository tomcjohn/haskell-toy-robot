module ToyRobot where

import Control.Monad.State
import System.IO

data Direction = North | East | South | West deriving Show

data Position = Position Int Int

data Robot = Robot Direction Position

type GameAction = StateT Robot IO ()

printRobot :: Robot -> String
printRobot (Robot d p) = printPosition p ++ "," ++ printDirection d

printPosition :: Position -> String
printPosition (Position x y) = show x ++ "," ++ show y

printDirection :: Direction -> String
printDirection = show

place :: Position -> Direction -> GameAction
place p d = put (Robot d p)

left :: GameAction
left = modify doLeft
  where
    doLeft (Robot North p) = Robot West p
    doLeft (Robot East p) = Robot North p
    doLeft (Robot South p) = Robot East p
    doLeft (Robot West p) = Robot South p

right :: GameAction
right = modify doRight
  where
    doRight (Robot North p) = Robot East p
    doRight (Robot East p) = Robot South p
    doRight (Robot South p) = Robot West p
    doRight (Robot West p) = Robot North p

move :: GameAction
move = modify doMove
  where
    doMove (Robot North (Position x y)) = Robot North (Position x (y + 1))
    doMove (Robot East (Position x y)) = Robot East (Position (x + 1) y)
    doMove (Robot South (Position x y)) = Robot South (Position x (y - 1))
    doMove (Robot West (Position x y)) = Robot West (Position (x - 1) y)

report :: GameAction
report = do
  r <- get
  liftIO (putStrLn (printRobot r))

parseInput :: String -> GameAction
parseInput line =
  if "place" == line
    then place (Position 1 4) South
  else if "left" == line
    then left
  else if "right" == line
    then right
  else if "move" == line
    then move
  else if "report" == line
    then report
  else
    return ()

runMe :: StateT Robot IO ()
runMe = do
  line <- liftIO myGetLine
  case line of
    Nothing -> return ()
    Just a -> do
      let op = parseInput a
      op
      runMe

myGetLine :: IO (Maybe String)
myGetLine = do
  eof <- hIsEOF stdin
  if eof
    then return Nothing
    else do
      input <- getLine
      return (Just input)

main :: IO ()
main = do
  let r = Robot North (Position 2 3)
  evalStateT runMe r
