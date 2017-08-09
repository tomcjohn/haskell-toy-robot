module Main where

import System.IO

import Control.Monad (forever, when)
import System.Exit (exitSuccess)

import Position
import Table

readLine :: Handle -> IO ()
readLine h = do
  line <- hGetLine h
  putStrLn line

readAllLines :: Handle -> Table -> IO ()
readAllLines h t = forever $ do
  weAreDone <- hIsEOF h
  when weAreDone exitSuccess
  cmd <- hGetLine h
  doCommand cmd t

main :: IO ()
main = do
  let t = Table (Position 0 0) (Position 5 5)

  let filename = "robot-test.in"
  h <- openFile filename ReadMode
  readAllLines h t
  hClose h
