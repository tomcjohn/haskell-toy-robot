module Main where

import Control.Monad.Trans.State

import Position
import Table

main :: IO ()
main = do
  let filename = "robot-test.in"
  content <- readFile filename

  let t = Table (Position 0 0) (Position 5 5)

  let states = (map (doCommand t) (lines content)) :: [GameState]

  let finalState = (foldr (>>) (pure ()) states) :: GameState

  evalStateT finalState Nothing
