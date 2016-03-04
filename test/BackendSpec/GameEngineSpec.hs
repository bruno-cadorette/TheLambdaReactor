{-# LANGUAGE OverloadedStrings #-}
module BackendSpec.GameEngineSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import GameEngine
import GameState (GameState(..))
import Character (Entity(..), Character (..))
import Data.Maybe
import Debug.Trace
import qualified Data.Text as Text
import qualified Data.Aeson as Aeson
import Linear.V2
import qualified Linear.Vector as LV
import qualified Data.Map.Strict as M

main :: IO()
main = hspec spec

spec :: Spec
spec = describe "GameEngine" $ do
        it "getGameStateForJSON empty" $ do
          (getGameStateForJSON getNewGameState) `shouldBe` (GameState M.empty [] [] [])

        it "getGameStateForJSON player and addPlayer" $ do
          (getGameStateForJSON $ addPlayer getNewGameState (Player "0" 15 (V2 1.0 1.0) (V2 1.0 1.0))) `shouldBe` (GameState  (M.fromList [(Text.pack "0",(Player "0" 15 (V2 1.0 1.0) (V2 1.0 1.0)))]) [] [] [])

        it "getPlayerTest true" $ do
          let x = addPlayer getNewGameState (Player "0" 15 (V2 1.0 1.0) (V2 1.0 1.0))
            in
              (isJust  (getPlayer x "0")) `shouldBe` True

        it "getPlayerTest false" $ do
          let x = addPlayer getNewGameState (Player "0" 15 (V2 1.0 1.0) (V2 1.0 1.0))
            in
              (isJust  (getPlayer x "23")) `shouldBe` False

        it "movePlayer" $ do
          let x = addPlayer getNewGameState (Player "0" 15 (V2 1.0 1.0) (V2 1.0 1.0))
            in
              position (fromJust  (getPlayer (handleControlV2 x (V2 0 1) "0") "0")) `shouldBe` (V2 1.0 2.0)

        it "intersecPlayer" $ do
          let y = addBullet  (addPlayer getNewGameState (Player "0" 15 (V2 1.0 1.0) (V2 1.0 1.0))) (V2 1.0 1.0) (V2 1.0 1.0) 50
            in
              (length $ getPlayersHit y) `shouldBe` 1
