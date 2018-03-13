module Main where

import            Data.List.Split (splitOn)

import            Debug.Trace     (trace)

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
  doCmds t cmds (pure r)

parseCommand :: String -> IO Command
parseCommand str = do
  let splitCmd = splitOn " " str
  let cmd = C.lookup (head splitCmd) (splitCmd!!1)
  pure cmd

doCmd :: Table -> Command -> IO Robot -> IO Robot
doCmd t (Place p d) ioR =
  if onTable t p
    then pure $ Robot p d
    else ioR
doCmd _ C.Move         ioR = trace "move" $ move ioR
doCmd _ C.Left         ioR = trace "left" $ left ioR
doCmd _ C.Right        ioR = trace "right" $ right ioR
doCmd _ C.Report       ioR = trace "report" $ report ioR
doCmd _ C.Unrecognised ioR = trace "na" ioR

doCmds :: Table -> [IO Command] -> IO Robot -> IO ()
doCmds _ [] _ = pure ()
doCmds t (c:cs) ioR = do
  cmd <- c
  let newR = doCmd t cmd ioR
  doCmds t cs newR
