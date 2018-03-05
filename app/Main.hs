module Main where

import           Control.Monad    (void)

import           Data.List.Split  (splitOn)

import qualified Command          as C

import           Direction        (Direction(..))

import           Robot            (Robot(..),
                                  move,
                                  left,
                                  right,
                                  report)

main :: IO ()
main = do
  let r = Robot East (2,3)
  let filename = "robot-test.in"
  content <- readFile filename
  let newR = (move . move . left . move . move . move . move . right . move . move) r
  void $ report newR

doCmd :: Robot -> String -> IO Robot
doCmd r c = do
  let splitCmd = splitOn " " c
  let cmd = C.lookup (splitCmd!!0) (splitCmd!!1)
  print cmd
  pure r
