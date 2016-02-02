{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}


module Message (Message (..),decodeMessage) where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy.Char8 as BS
import GHC.Generics

data Message = Message
    { name :: Text.Text
    , body :: Text.Text
    } deriving (Generic,Show)

type ServerMessage =  Text.Text
    
instance Aeson.ToJSON Message

instance Aeson.FromJSON Message where
  parseJSON  = Aeson.genericParseJSON Aeson.defaultOptions

decodeMessage :: Text.Text -> Maybe Message
decodeMessage text = Aeson.decode (BS.pack (Text.unpack text))
