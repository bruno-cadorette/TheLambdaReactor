{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}


module Message (Message (..),decodeMessage) where

import Data.Aeson
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy.Char8 as BS
import GHC.Generics

data Message = Message
    { name :: Text.Text
    , body :: Text.Text
    } deriving (Generic,Show)

type ServerMessage =  Text.Text
    
instance ToJSON Message

instance FromJSON Message

decodeMessage :: Text.Text -> Maybe Message
decodeMessage text = decode (BS.pack (Text.unpack text))
