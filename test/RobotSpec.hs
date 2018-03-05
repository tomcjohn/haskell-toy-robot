module RobotSpec (spec) where

import Direction   (Direction (..))
import Position    (Position)
import Robot       (Robot (..),
                    move)
import Test.Hspec  (Spec, Expectation, describe, it, shouldBe)

spec :: Spec
spec =
  describe "move" $ do
    it "moves the robot one space relative to its direction" $ do
      checkRobot (move $ Robot North (2,5)) North (2,6)
      checkRobot (move $ Robot East  (2,5)) East  (3,5)
      checkRobot (move $ Robot South (2,5)) South (2,4)
      checkRobot (move $ Robot West  (2,5)) West  (1,5)

checkRobot :: IO Robot -> Direction -> Position -> Expectation
checkRobot ioR o p = do
  r <- ioR
  direction r `shouldBe` o
  position    r `shouldBe` p
