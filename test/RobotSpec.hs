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
      checkRobot (move $ Robot (2,5) North) North (2,6)
      checkRobot (move $ Robot (2,5) East)  East  (3,5)
      checkRobot (move $ Robot (2,5) South) South (2,4)
      checkRobot (move $ Robot (2,5) West)  West  (1,5)

checkRobot :: Robot -> Direction -> Position -> Expectation
checkRobot r o p = do
  direction r `shouldBe` o
  position  r `shouldBe` p
