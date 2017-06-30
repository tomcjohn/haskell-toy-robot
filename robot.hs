import Control.Monad.State
import System.IO
import Data.List
import Data.String

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
parseInput cmd = do
  let splitCmd = mySplit ' ' cmd
  case (splitCmd !! 0) of
    "PLACE" -> do
      let splitPlaceCmd = mySplit ',' (splitCmd !! 1)
      let newX = read (splitPlaceCmd !! 0) :: Int
      let newY = read (splitPlaceCmd !! 1) :: Int
      let newPos = Position newX newY
      let newDir = lookupDir (splitPlaceCmd !! 2)
      place newPos newDir
    "LEFT" -> left
    "RIGHT" -> right
    "MOVE" -> move
    "REPORT" -> report
    _ -> return()

mySplit :: Char -> [Char] -> [[Char]]
mySplit c [] = []
mySplit c s = doSplit c s []

doSplit :: Char -> [Char] -> [[Char]] -> [[Char]]
doSplit c [] acc = acc
doSplit c s acc = do
  let i = findIndex (\x -> x == c) s
  case i of
    Nothing -> doSplit c [] (acc ++ [s])
    Just a -> doSplit c (drop (a+1) s) (acc ++ [take a s])

lookupDir :: String -> Direction
lookupDir d = do
  case d of
    "NORTH" -> North
    "SOUTH" -> South
    "EAST" -> East
    "WEST" -> West

myGetLine :: Handle -> IO (Maybe String)
myGetLine handle = do
  eof <- hIsEOF handle
  if eof
    then return Nothing
    else do
      line <- hGetLine handle
      return (Just line)

runMe :: Handle -> GameAction
runMe handle = do
  line <- liftIO (myGetLine handle)
  case line of
    Nothing -> return ()
    Just a -> do
      let op = parseInput a
      op
      runMe handle

main :: IO ()
main = do
  handle <- openFile "robot-test.in" ReadMode
  let r = Robot North (Position 2 3)
  evalStateT (runMe handle) r
  hClose handle
