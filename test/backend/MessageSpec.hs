{-# LANGUAGE OverloadedStrings #-}
module MessageSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Message (Message (..),decodeMessage)
import Data.Maybe
import Debug.Trace
import qualified Data.Text as Text
import qualified Data.Aeson as Aeson

main :: IO()
main = hspec spec

spec :: Spec
spec = describe "Message" $ do
        it "decodeMessage" $ do
          (isJust (decodeMessage  (Text.pack $ "{\"name\":\"allo\",\"body\":\"allo\"}"))) `shouldBe` True

        it "fail" $ do
         False `shouldBe` True
