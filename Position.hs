module Position where

data Position = Position Integer Integer

instance Show Position where
  show (Position x y) = (show x) ++ "," ++ (show y)
