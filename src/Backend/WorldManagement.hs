{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module WorldManagement (worldManager,worldSender, UserInput, WorldObject) where

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
type WorldObject = WorldEngine

--TODO move to helper
toId :: ByteString -> Id
toId = decodeUtf8

--TODO do shooter
data UserInput = Movement Move Socket | Shoot Socket | Both Move Socket Socket
instance GetSocket UserInput where
    getSocket (Movement _ s) = s
    getSocket (Shoot s) = s
    getSocket (Both _ s _) = s


worldManager :: (MonadIO m, MonadState RoutingTable m) => m (MomentIO (Event UserInput, Behavior WorldObject))
worldManager = do
    userInputSocket <- createSocketEvent "userInput"
    return $ do
        userInput  <-  userInputSocket

        let inputEvent = (\(s, n) -> Movement n s) <$> userInput

        mapAccum getNewWorld $ fmap input $ inputEvent
    where
        input (Movement n s) m = ((Movement n s), handleControlV2 m n (toId (socketId s)))
        --input (Shoot s) m = ((Shoot s), Map.delete s m)
        --input (Both n s s') m = ((Both n s s'), Map.insert s n $ Map.delete s' m)

--Dont really need it right now
notifyMove n = broadcastAll "ACK" n

worldSender :: (MonadIO m) => WorldObject -> UserInput -> ReaderT Socket m ()
worldSender x (Movement n s) = notifyMove n
