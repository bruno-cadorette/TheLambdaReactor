{-# LANGUAGE OverloadedStrings #-}
module BackendSpec.GamingSpec.HelperSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Game.Helper
import Data.Maybe
import Debug.Trace
import Linear.V2

firstLocation = (Location (V2 1.0 1.0) (V2 1.0 0.0))


main :: IO()
main = hspec spec

spec :: Spec
spec = describe "HelpereSpec" $ do
        it "add" $ do
          add firstLocation (Location (V2 2.0 2.0) (V2 0.0 1.0) ) `shouldBe` (Location (V2 3.0 3.0) (V2 0.0 1.0))
        it "add nothin" $ do
          add firstLocation (Location (V2 0.0 0.0) (V2 0.0 1.0) ) `shouldBe` (Location (V2 1.0 1.0) (V2 0.0 1.0))
        it "moveLocation" $ do
          moveLocation firstLocation (V2 1.0 0.0) `shouldBe` (Location (V2 2.0 1.0) (V2 1.0 0.0))
        it "changeOri" $ do
          changeOri firstLocation (V2 0.0 1.0) `shouldBe` (Location (V2 1.0 1.0) (V2 0.0 1.0))
        it "normalize" $ do
          normalize (V2 0.0 3.0) `shouldBe` (V2 0 1)
        it "normalize zero" $ do
          normalize (V2 0.0 0.0) `shouldBe` (V2 0 0)
        it "normalize nega" $ do
          normalize (V2 (negate 9.0) 0.0) `shouldBe` (V2 (negate 1) 0)
        it "divide" $ do
          divide 2 (V2 0.0 3.0) `shouldBe` (V2 0 1.5)
        it "divide zero" $ do
          divide 0 (V2 5.0 5.0) `shouldBe` (V2 0 0)
        it "divide zero 2" $ do
          divide 3 (V2 0.0 0.0) `shouldBe` (V2 0 0)
        it "divide nega" $ do
          divide 3 (V2 (negate 9.0) 0.0) `shouldBe` (V2 (negate 3) 0)
