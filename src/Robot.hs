module Robot where

import qualified Direction  as O
import           Direction  (Direction)

import qualified Position   as P
import           Position   (Position)

data Robot = Robot
  { position::Position
  , direction::Direction
  } deriving Show

left :: IO Robot -> IO Robot
left ioR = do
  r <- ioR
  pure $ Robot (position r) (O.left $ direction r)

right :: IO Robot -> IO Robot
right ioR = do
  r <- ioR
  pure $ Robot (position r) (O.right $ direction r)

move :: IO Robot -> IO Robot
move ioR = do
  r <- ioR
  pure $ Robot (P.move (direction r) (position r)) (direction r)

report :: IO Robot -> IO Robot
report ioR = do
  r <- ioR
  print $ (show $ direction r) ++ " " ++ (show $ position r)
  pure r
