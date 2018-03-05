module Pos where

import Orientation

type Pos = (Int,Int)

move :: Orientation -> Pos -> Pos
move North (x,y) = (x  , y+1)
move East  (x,y) = (x+1, y  )
move South (x,y) = (x  , y-1)
move West  (x,y) = (x-1, y  )
