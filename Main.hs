module Main where

import System.IO

import Control.Monad (forever, when)
import System.Exit (exitFailure, exitSuccess)

import Direction
import Position
import Robot
import Table

readLine :: Handle -> IO ()
readLine h = do
  line <- hGetLine h
  putStrLn line

readAllLines :: Handle -> IO ()
readAllLines h = forever $ do
  weAreDone <- hIsEOF h
  when weAreDone exitSuccess

  cmd <- hGetLine h
  Table.doCommand cmd

main :: IO ()
main = do
  let filename = "robot-test.in"
  h <- openFile filename ReadMode
  readAllLines h
  hClose h
