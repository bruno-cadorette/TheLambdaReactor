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
import Lib
import qualified Linear.Vector as LV
import qualified Data.Map.Strict as M
import Data.ByteString.Char8 as BS

world = getNewGameState
emptyGameState = (getGameStateForJSON (getNewGameState, M.empty))

main :: IO()
main = hspec spec

spec :: Spec
spec = describe "GameEngine" $ do
        it "getGameStateForJSON empty" $ do
          (getGameStateForJSON (getNewGameState, M.empty)) `shouldBe` (GameState M.empty M.empty [] [])

        it "getGameStateForJSON player and addPlayer" $ do
          let world = (getGameStateForJSON $ ((addBullet getNewGameState (Location (V2 1.0 1.0) (V2 1.0 1.0)) 50 "12" ), M.empty))
           in
            (Prelude.length$ M.elems $projectiles world)  == 1 `shouldBe` True
        it " addBullet" $ do
          (getGameStateForJSON  ((addBullet world (Location (V2 1 1) (V2 1 0)) 50 "lol"), M.empty)) `shouldNotBe` emptyGameState
