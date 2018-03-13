module Command where

import           Data.List.Split  (splitOn)

import qualified Direction        as D
import           Direction        (Direction)

import           Position         (Position)

data Command
  = Place Position Direction
  | Move
  | Left
  | Right
  | Report
  | Unrecognised
  deriving Show

lookup :: String -> String -> Command
lookup "PLACE"  s = do
  let split = splitOn "," s
  let x = read $ head split
  let y = read $ split!!1
  let d = D.lookup $ split!!2
  Place (x,y) d
lookup "MOVE"   _ = Move
lookup "LEFT"   _ = Command.Left
lookup "RIGHT"  _ = Command.Right
lookup "REPORT" _ = Report
lookup _ _        = Unrecognised
