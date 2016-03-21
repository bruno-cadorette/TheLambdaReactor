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
import Lib
import Data.Time.Clock
import Data.Time
import Data.Time.Format

main :: IO()
main = hspec spec

spec :: Spec
spec = describe "Bullet" $ do
        it "moveBullet" $ do
          let dateString = "26 Jan 2012 1:00 AM"
          let timeFromString = readTime defaultTimeLocale "%d %b %Y %l:%M %p" dateString :: UTCTime -- 3600000 milli
           in
            moveBullet (Bullet 3599000 (Location (V2 1 1) (V2 1 0)) 0.001 "12" ) timeFromString `shouldBe` (Bullet 3599000 (Location (V2 2 1) (V2 1 0)) 0.001 "12" )
