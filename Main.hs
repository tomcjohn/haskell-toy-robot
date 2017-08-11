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

printLength :: String -> [a] -> IO ()
printLength s xs = print (s ++ ": " ++ (show (length xs)))

main :: IO ()
main = do
  let t = Table (Position 0 0) (Position 5 5)

  let filename = "robot-test.in"
  content <- readFile filename

  let cmds = lines content
  printLength "cmds" cmds

  let states = map (\s -> doCommand s t) (lines content)
  printLength "states" states

  let finalState = foldr (>>) (pure ()) states

  print (execState finalState (Robot (Position 1 1) North))
