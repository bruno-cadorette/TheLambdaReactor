{-# LANGUAGE OverloadedStrings #-}
module BackendSpec.WorldEngineSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import WorldEngine(getWorldForJSON, getNewWorld, addPlayer,removePlayer,damageToPlayer,movePlayer, WorldEngine (..))
import World (World(..))
import Character (Player(..),Ennemy(..), Character (..))
import Data.Maybe
import Debug.Trace
import qualified Data.Text as Text
import qualified Data.Aeson as Aeson
import Linear.V2
import qualified Linear.Vector as LV

main :: IO()
main = hspec spec

spec :: Spec
spec = describe "WorldEngine" $ do
        it "getWorldForJSON empty" $ do
          (getWorldForJSON getNewWorld) `shouldBe` (World [] [] [] [])

        it "getWorldForJSON player and addPlayer" $ do
          (getWorldForJSON $ addPlayer getNewWorld (Player 0 15 (V2 1.0 1.0) (V2 1.0 1.0))) `shouldBe` (World [(Player 0 15 (V2 1.0 1.0) (V2 1.0 1.0))] [] [] [])
