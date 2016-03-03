{-# LANGUAGE OverloadedStrings #-}

module Game.Helper (normalize) where
  import qualified Data.Aeson as Aeson
  import Linear.V2
  import Linear.Vector
  import Control.Lens


  normalize :: V2 Float -> V2 Float
  normalize v = (1/ magnitude) *^ v
                where magnitude = sqrt ((v ^._x) ** 2 + (v ^._y) ** 2)


  instance (Aeson.ToJSON a) => Aeson.ToJSON (V2 a)  where
   toJSON x =
      Aeson.object [ "x"  Aeson..= (x ^._x)
                   , "y"   Aeson..= (x ^._y)
                     ]

  instance (Aeson.FromJSON a) => Aeson.FromJSON (V2 a) where
  parseJSON (Aeson.Object v) =
     V2     <$> v Aeson..: "x"
            <*> v Aeson..: "y"
