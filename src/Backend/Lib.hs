{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib (module Exportable, decodeMessage) where
  import Elm.Module
  import Elm.Derive
  import Elm.TyRep
  import GameState
  import Game.Helper as Exportable
  import Data.Proxy
  import Character
  import Bullet
  import Data.Maybe as Exportable
  import Message
  import qualified Data.Aeson as Aeson
  import Linear.V2 as Exportable
  import Control.Lens as Exportable
  import qualified Data.ByteString.Lazy.Char8 as BS
  import Data.Text as Text

  Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''GameState
  Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''Location
  Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''Entity
  Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''Bullet
  Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''Hit
  Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''Message

  decodeMessage :: Text.Text -> Maybe Message
  decodeMessage text = Aeson.decode (BS.pack (Text.unpack text))

  instance (Aeson.ToJSON a) => Aeson.ToJSON (V2 a)  where
   toJSON x =
      Aeson.object [ "x"  Aeson..= (x ^._x)
                   , "y"   Aeson..= (x ^._y)
                     ]

  instance (Aeson.FromJSON a) => Aeson.FromJSON (V2 a) where
  parseJSON (Aeson.Object v) =
     V2     <$> v Aeson..: "x"
            <*> v Aeson..: "y"