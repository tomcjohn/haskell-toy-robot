module Direction where

import Position

data Direction = North | South | East | West

instance Show Direction where
  show North = "North"
  show South = "South"
  show East = "East"
  show West = "West"

move :: Direction -> Position -> Position
move North (Position x y) = Position x (y+1)
move South (Position x y) = Position x (y-1)
move East  (Position x y) = Position (x+1) y
move West  (Position x y) = Position (x-1) y

left :: Direction -> Direction
left North = West
left West = South
left South = East
left East = North

right :: Direction -> Direction
right North = East
right East = South
right South = West
right West = North

lookup :: String -> Direction
lookup "NORTH" = North
lookup "SOUTH" = South
lookup "EAST" = East
lookup "WEST" = West
lookup _ = undefined
