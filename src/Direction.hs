module Direction where

data Direction
  = North
  | West
  | South
  | East
  | Unrecognised
  deriving (Eq, Show)

left :: Direction -> Direction
left North = West
left East  = North
left South = East
left West  = South

right :: Direction -> Direction
right North = East
right East  = South
right South = West
right West  = North

lookup :: String -> Direction
lookup "NORTH" = North
lookup "WEST"  = West
lookup "SOUTH" = South
lookup "EAST"  = East
lookup _       = Unrecognised
