module Direction where

data Direction = North | South | East | West

instance Show Direction where
  show North = "North"
  show South = "South"
  show East = "East"
  show West = "West"

turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft West = South
turnLeft South = East
turnLeft East = North

turnRight :: Direction -> Direction
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North
