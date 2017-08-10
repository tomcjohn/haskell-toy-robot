module Main where

import System.IO

import Command
import Position
import Robot
import Table

readLine :: Handle -> IO ()
readLine h = do
  line <- hGetLine h
  putStrLn line

printRobots :: [IO Robot] -> IO ()
printRobots [] = putStrLn "done"
printRobots (i:is) = do
  r <- i
  putStrLn (show r)
  printRobots is

main :: IO ()
main = do
  let t = Table (Position 0 0) (Position 5 5)

  let filename = "robot-test.in"
  content <- readFile filename
  let cmds = map toCommand (lines content)

  let robots = map (\cmd -> doCommand cmd t) cmds
  printRobots robots
