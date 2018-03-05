module Orientation where

data Orientation
  = North
  | South
  | East
  | West
  deriving (Eq, Show)

left :: Orientation -> Orientation
left North = West
left East  = North
left South = East
left West  = South

right :: Orientation -> Orientation
right North = East
right East  = South
right South = West
right West  = North
