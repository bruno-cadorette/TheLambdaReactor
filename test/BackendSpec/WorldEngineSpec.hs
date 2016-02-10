{-# LANGUAGE OverloadedStrings #-}
module BackendSpec.WorldEngineSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import WorldEngine
import World (World(..))
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
spec = describe "WorldEngine" $ do
        it "getWorldForJSON empty" $ do
          (getWorldForJSON getNewWorld) `shouldBe` (World [] [] [] [])

        it "getWorldForJSON player and addPlayer" $ do
          (getWorldForJSON $ addPlayer getNewWorld (Player "0" 15 (V2 1.0 1.0) (V2 1.0 1.0))) `shouldBe` (World [(Player "0" 15 (V2 1.0 1.0) (V2 1.0 1.0))] [] [] [])

        it "getPlayerTest true" $ do
          let x = addPlayer getNewWorld (Player "0" 15 (V2 1.0 1.0) (V2 1.0 1.0))
            in
              (isJust  (getPlayer x "0")) `shouldBe` True

        it "getPlayerTest false" $ do
          let x = addPlayer getNewWorld (Player "0" 15 (V2 1.0 1.0) (V2 1.0 1.0))
            in
              (isJust  (getPlayer x "23")) `shouldBe` False

        it "movePlayer" $ do
          let x = addPlayer getNewWorld (Player "0" 15 (V2 1.0 1.0) (V2 1.0 1.0))
            in
              position (fromJust  (getPlayer (handleControl x "w" "0") "0")) `shouldBe` (V2 1.0 2.0)

        it "intersecPlayer" $ do
          let y = addBullet  (addPlayer getNewWorld (Player "0" 15 (V2 1.0 1.0) (V2 1.0 1.0))) (V2 1.0 1.0) (V2 1.0 1.0) 50
            in
              (length $ getPlayersHit y) `shouldBe` 1
