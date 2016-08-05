module ToyRobot where

import Control.Monad ((>=>))

data Direction = North | East | South | West

data Position = Position Int Int

data Robot = Robot Direction Position

printRobot :: Robot -> String
printRobot (Robot d p) = printPosition p ++ "," ++ printDirection d

printPosition :: Position -> String
printPosition (Position x y) = show x ++ "," ++ show y

printDirection :: Direction -> String
printDirection North = "North"
printDirection East = "East"
printDirection South = "South"
printDirection West = "West"

place :: Position -> Robot -> IO Robot
place p (Robot d _) = return (Robot d p)

turnLeft :: Robot -> IO Robot
turnLeft (Robot North p) = return (Robot West p)
turnLeft (Robot East p) = return (Robot North p)
turnLeft (Robot South p) = return (Robot East p)
turnLeft (Robot West p) = return (Robot South p)

turnRight :: Robot -> IO Robot
turnRight (Robot North p) = return (Robot East p)
turnRight (Robot East p) = return (Robot South p)
turnRight (Robot South p) = return (Robot West p)
turnRight (Robot West p) = return (Robot North p)

move :: Robot -> IO Robot
move (Robot North (Position x y)) = return (Robot North (Position x (y + 1)))
move (Robot East (Position x y)) = return (Robot East (Position (x + 1) y))
move (Robot South (Position x y)) = return (Robot South (Position x (y - 1)))
move (Robot West (Position x y)) = return (Robot West (Position (x - 1) y))

report :: Robot -> IO Robot
report r = do
  putStrLn $ printRobot(r)
  return r

main :: IO Robot
main = do
  (move >=> report >=>
   move >=> report >=>
   place (Position 1 1) >=> report >=>
   turnLeft >=> report >=>
   move >=> report >=>
   turnLeft >=> report >=>
   move >=> report) r
  where r = Robot North (Position 2 3)
