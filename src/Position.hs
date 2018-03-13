module Position where

import Direction

type Position = (Int,Int)

move :: Direction -> Position -> Position
move North (x,y) = (x  , y+1)
move East  (x,y) = (x+1, y  )
move South (x,y) = (x  , y-1)
move West  (x,y) = (x-1, y  )
move _     pos   = pos
