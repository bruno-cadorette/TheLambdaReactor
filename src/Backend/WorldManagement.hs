{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module WorldManagement (worldManager,worldSender, UserInput) where

import Reactive
import qualified Data.Map as Map
import Control.Monad.State.Strict
import Control.Monad.Reader
import Data.Text
import Data.Aeson
import Network.EngineIO (SocketId)
import Network.SocketIO
import Data.ByteString.Char8
import Linear.V2
import Linear.V
import Data.Text.Encoding
import Character
import WorldEngine

type Move = V2 Float
type Direction = V2 Float

--TODO move to helper
toId :: ByteString -> Id
toId = decodeUtf8

--TODO do shooter
data UserInput = Movement Move Socket | Shoot Direction Socket | Both Move Direction Socket Socket
instance GetSocket UserInput where
    getSocket (Movement _ s) = s
    getSocket (Shoot _ s) = s
    getSocket (Both _ _ s _) = s


worldManager :: (MonadIO m, MonadState RoutingTable m) => m (MomentIO (Event UserInput, Behavior WorldEngine))
worldManager = do
    userInputSocket <- createSocketEvent "userInput"
    userShootSocket <- createSocketEvent "userShoot"
    return $ do
        userInput  <-  userInputSocket
        userShoot <- userShootSocket
        let inputEvent = (\(s, n) -> Movement n s) <$> userInput
        let shootEvent = (\(s, n) -> Shoot n s) <$> userShoot

        mapAccum getNewWorld $ fmap input $ unionWith (\(Movement n s) (Shoot d s') -> Both n d s s') inputEvent shootEvent
    where
        input (Movement n s) m = ((Movement n s), handleControlV2 m n (toId (socketId s)))
        input (Shoot d s) m = ((Shoot d s), handleShoot d (toId (socketId s)) m)
        input (Both n d s s') m = ((Both n d s s'), handleShoot d (toId (socketId s')) $ handleControlV2 m n (toId (socketId s)))

--Dont really need it right now
notifyMove :: (MonadIO m) => WorldEngine -> ReaderT Socket m ()
notifyMove n = broadcastAll "updateWorld" (getWorldForJSON n)

worldSender :: (MonadIO m) => WorldEngine -> UserInput -> ReaderT Socket m ()
worldSender x _ = notifyMove x
