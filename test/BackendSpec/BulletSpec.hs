{-# LANGUAGE OverloadedStrings #-}
module BackendSpec.BulletSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Bullet (Bullet(..),moveBullet)
import Data.Maybe
import Debug.Trace
import qualified Data.Text as Text
import qualified Data.Aeson as Aeson
import Linear.V2
import qualified Linear.Vector as LV

main :: IO()
main = hspec spec

spec :: Spec
spec = describe "Bullet" $ do
        it "moveBullet" $ do
          moveBullet (Bullet 50 (V2 1 1) (V2 1 0) 5 50) `shouldBe` (Bullet 50 (V2 6 1) (V2 1 0) 5 50)
