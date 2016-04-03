{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib (module Exportable, decodeMessage, ApiExample(..)) where
  import Elm.Derive
  import GameState
  import Game.Helper as Exportable
  import Character
  import Bullet
  import Data.Maybe as Exportable
  import Message
  import Data.Aeson as Aeson
  import Linear.V2 as Exportable
  import Control.Lens as Exportable
  import qualified Data.ByteString.Lazy.Char8 as BS
  import Data.Text as Text
  import Control.Monad
  import Game.MapReader
  type Move = V2 Float
  type Direction = V2 Float

  data ApiExample = Connection String | MovementIn Move | ShootIn Direction | Disconnection | ChatIn Text.Text deriving (Show)

  Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''Bullet
  Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''GameState
  Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''Location
  Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''Entity
  Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''Hit
  Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''Message
  Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''GameMap


  decodeMessage :: Text.Text -> Maybe Message
  decodeMessage text = Aeson.decode (BS.pack (Text.unpack text))

  instance (Aeson.ToJSON a) => Aeson.ToJSON (V2 a)  where
   toJSON x =
      Aeson.object [ "x"  Aeson..= (x ^._x)
                   , "y"   Aeson..= (x ^._y)
                     ]

  instance FromJSON a => FromJSON (V2 a) where
    parseJSON (Object obj) = V2 <$> obj .: "x" <*> obj .: "y"
    parseJSON _ = mzero
