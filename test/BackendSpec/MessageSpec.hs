{-# LANGUAGE OverloadedStrings #-}
module BackendSpec.MessageSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Message (Message (..))
import Data.Maybe
import Debug.Trace
import qualified Data.Text as Text
import qualified Data.Aeson as Aeson
import Lib

main :: IO()
main = hspec spec

spec :: Spec
spec = describe "Message" $ do
        it "decodeMessage" $ do
          (isJust (decodeMessage  (Text.pack $ "{\"name\":\"allo\",\"body\":\"allo\"}"))) `shouldBe` True

        it "decodeMessage fail" $ do
         (isJust (decodeMessage  (Text.pack $ "{\"patate\":\"allo\",\"lol\":\"allo\"}"))) `shouldBe` False
