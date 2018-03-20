module Main where

import           Control.Monad.Trans.State
import           Table

main :: IO ()
main = do
  let t = Table (0,0) (5,5)

  let filename = "robot-test.in"
  content <- readFile filename

  let states = doCommand t <$> lines content
  let finalState = foldr (>>) (pure ()) states
  _ <- execStateT finalState Nothing
  pure ()
