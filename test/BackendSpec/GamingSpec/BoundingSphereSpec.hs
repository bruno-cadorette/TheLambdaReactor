{-# LANGUAGE OverloadedStrings #-}
module BackendSpec.GamingSpec.BoundingSphereSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Game.BoundingSphere
import Data.Maybe
import Debug.Trace
import Linear.V2

main :: IO()
main = hspec spec

spec :: Spec
spec = describe "BoundingSphereSpec" $ do
        it "interSect Same" $ do
          intersecting (BoundingSphere (V2 0.0 0.0) 1) (BoundingSphere (V2 0.0 0.0) 1) `shouldBe` True
        it "interSect False" $ do
          intersecting (BoundingSphere (V2 0.0 0.0) 1) (BoundingSphere (V2 5.0 0.2) 1) `shouldBe` False
        it "interSect right" $ do
          intersecting (BoundingSphere (V2 0.0 0.0) 1) (BoundingSphere (V2 0.5 0.2) 1) `shouldBe` True
        it "interSect left" $ do
          intersecting (BoundingSphere (V2 (-0.5) 0.1) 1) (BoundingSphere (V2 0.0 0.0) 1) `shouldBe` True
        it "intersectBoxPos True" $ do
          intersectBoxPos (V2 0 0) (V2 1 0) 1.0 1.0 `shouldBe` True
        it "intersectBoxPos False" $ do
          intersectBoxPos (V2 5 0) (V2 0 0) 1.0 1.0 `shouldBe` False
        it "intersectBoxPos Same" $ do
          intersectBoxPos (V2 0 0) (V2 0 0) 1.0 0.5 `shouldBe` True
