{-# LANGUAGE OverloadedStrings #-}
module MessageSpec where

import Test.Hspec (Spec, hspec, describe, it, shouldSatisfy)
import Message (Message (..),decodeMessage)
import qualified Data.Aeson as Aeson

spec :: Spec
spec = describe "Message" $ do
        it "decodeMessage" $ do
          (isJust (decodeMessage (Aeson.encode (Message "allo" "allo") ))) `shouldBe` True

      it "fail" $ do
        False `shouldBe` True

main :: IO()
main = hspec spec
