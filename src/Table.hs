module Table where

import            Position        (Position)

data Table = Table Position Position

onTable :: Table -> Position -> Bool
onTable (Table (x1,y1) (x2,y2)) (x,y) =
  x >= x1 && x <= x2 && y >= y1 && y <= y2

offTable :: Table -> Position -> Bool
offTable t p = not $ onTable t p
