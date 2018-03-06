module Main where

import            Control.Monad   (void)

import            Data.List.Split (splitOn)

import qualified  Command         as C
import            Command         (Command (..))

import            Direction       (Direction (..))

import            Robot           (Robot (..),
                                   move,
                                   left,
                                   right,
                                   report)
import            Table           (Table (..),
                                   onTable)

main :: IO ()
main = do
  let r = Robot (2,3) East
  let t = Table (0,0) (9,9)
  let filename = "robot-test.in"
  content <- readFile filename
  let cmds = parseCommand <$> lines content
  let c1 = cmds!!0 :: Command
  void $ doCmd t c1 (pure r)

parseCommand :: String -> Command
parseCommand s =
  let splitCmd = splitOn " " s
  in C.lookup (splitCmd!!0) (splitCmd!!1)

doCmd :: Table -> Command -> IO Robot -> IO Robot
doCmd t (Place p d) ioR = do
  if onTable t p
    then pure $ Robot p d
    else ioR
doCmd _ C.Move         ioR = move ioR
doCmd _ C.Left         ioR = left ioR
doCmd _ C.Right        ioR = right ioR
doCmd _ C.Report       ioR = report ioR
doCmd _ C.Unrecognised ioR = ioR
