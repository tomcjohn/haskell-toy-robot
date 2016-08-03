module ToyRobot where

data Direction = North | East | South | West

data Position = Position Int Int

data Robot = Robot Direction Position
instance Show Robot where
  show r = printRobot r

place :: Position -> Robot -> Robot
place p (Robot d _) = Robot d p

turnLeft :: Robot -> Robot
turnLeft (Robot North p) = Robot West p
turnLeft (Robot East p) = Robot North p
turnLeft (Robot South p) = Robot East p
turnLeft (Robot West p) = Robot South p

turnRight :: Robot -> Robot
turnRight (Robot North p) = Robot East p
turnRight (Robot East p) = Robot South p
turnRight (Robot South p) = Robot West p
turnRight (Robot West p) = Robot North p

move :: Robot -> Robot
move (Robot North (Position x y)) = Robot North (Position x (y + 1))
move (Robot East (Position x y)) = Robot East (Position (x + 1) y)
move (Robot South (Position x y)) = Robot South (Position x (y - 1))
move (Robot West (Position x y)) = Robot West (Position (x - 1) y)

printDirection :: Direction -> String
printDirection North = "North"
printDirection East = "East"
printDirection South = "South"
printDirection West = "West"

printPosition :: Position -> String
printPosition (Position x y) = show x ++ "," ++ show y

printRobot :: Robot -> String
printRobot (Robot d p) = (printPosition p) ++ "," ++ (printDirection d)

report :: Robot -> IO Robot
report r = do
  putStrLn $ printRobot(r)
  return r

main :: IO ()
main = do
  report r
  print ((move . move . turnLeft . move . turnLeft . move . move . move) r)
  where r = Robot North (Position 2 3)
