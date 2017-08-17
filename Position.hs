module Position where

data Position = Position Int Int

instance Show Position where
  show (Position x y) = show x ++ "," ++ show y
