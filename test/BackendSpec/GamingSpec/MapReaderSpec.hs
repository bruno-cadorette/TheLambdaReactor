{-# LANGUAGE OverloadedStrings #-}
module BackendSpec.GamingSpec.MapReaderSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Game.MapReader
import Data.Maybe
import Debug.Trace
import Linear.V2
import Data.Map.Strict as Map

main :: IO()
main = hspec spec

spec :: Spec
spec = describe "MapReaderSpec" $ do
          it "parseMap" $ do
            mapo <- parseMap "test/test.dat"
            Map.elems mapo `shouldNotBe` []
          it "mapToExport size" $ do
            mapo <- parseMap "test/test.dat"
            taille (mapToExport mapo) `shouldBe` (8,3)
          it "mapToExport items" $ do
            mapo <- parseMap "test/test.dat"
            length (items (mapToExport mapo)) `shouldBe` (8*3)
