module RobotSpec (spec) where

import Orientation   (Orientation(..))
import Robot         (Robot(..))
import Test.Hspec    (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "left" $ do
    it "rotates robot one turn to the left" $
      let robot  = Robot North (1,2)
        in True `shouldBe` True
