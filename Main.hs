module Main where

import Control.Monad.State
import System.IO

import Direction
import Position
import Robot
import Table

readLine :: Handle -> IO ()
readLine h = do
  line <- hGetLine h
  putStrLn line

main :: IO ()
main = do
  let t = Table (Position 0 0) (Position 5 5)

  let filename = "robot-test.in"
  content <- readFile filename

  let states = map (\s -> doCommand s t) (lines content)
  let finalState = foldr (>>) (pure ()) states

  print (execState finalState (Robot (Position 1 1) North))
