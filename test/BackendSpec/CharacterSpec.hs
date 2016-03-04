{-# LANGUAGE OverloadedStrings #-}
module BackendSpec.CharacterSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Character (Entity(..), Character (..))
import Data.Maybe
import Debug.Trace
import qualified Data.Text as Text
import qualified Data.Aeson as Aeson
import Lib
import Linear.V2
import qualified Linear.Vector as LV

main :: IO()
main = hspec spec

spec :: Spec
spec = describe "Character" $ do
        it "hp" $ do
          hp (Entity 15 (Location (V2 1 1) (V2 1 1))) `shouldBe` 15
        it "position" $ do
          (position $ location (Entity 15 (Location (V2 1 1) (V2 1 1)))) `shouldBe` (V2 1 1)
        it "isDead" $ do
          isDead (Entity 15 (Location (V2 1 1) (V2 1 1))) `shouldBe` False
        it "isDead False" $ do
          isDead (Entity 0 (Location (V2 1 1) (V2 1 1))) `shouldBe` True
        it "isDead Neg" $ do
          isDead (Entity (0-5) (Location (V2 1 1) (V2 1 1))) `shouldBe` True
        it "move" $ do
          (position $ location (move (Entity 15 (Location (V2 1 1) (V2 1 1))) (V2 1.0 1.0))) `shouldBe` ((V2 2 2) :: V2 Float)
