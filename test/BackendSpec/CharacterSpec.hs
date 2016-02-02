{-# LANGUAGE OverloadedStrings #-}
module BackendSpec.CharacterSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Character (Player(..),Enemy(..), Character (..))
import Data.Maybe
import Debug.Trace
import qualified Data.Text as Text
import qualified Data.Aeson as Aeson
import Linear.V2
import qualified Linear.Vector as LV

main :: IO()
main = hspec spec

spec :: Spec
spec = describe "Character" $ do
        it "hp" $ do
          hp (Player 0 15 (V2 1 1) (V2 1 1)) `shouldBe` 15
        it "position" $ do
          position (Player 0 15 (V2 1 1) (V2 1 1)) `shouldBe` (V2 1 1)
        it "isDead" $ do
          isDead (Player 0 15 (V2 1 1) (V2 1 1)) `shouldBe` False
        it "isDead False" $ do
          isDead (Player 0 0 (V2 1 1) (V2 1 1)) `shouldBe` True
        it "isDead Neg" $ do
          isDead (Player 0 (0-5) (V2 1 1) (V2 1 1)) `shouldBe` True
        it "move" $ do
          (position (move (Player 0 15 (V2 1.0 1.0) (V2 1.0 1.0)) (V2 1.0 1.0))) `shouldBe` ((V2 2 2) :: V2 Float)
