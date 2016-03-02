{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module GameStateManagement (gameStateManager,gameStateSender, UserInput,fpsClock) where

import Reactive
import qualified Data.Map as Map
import Control.Monad.State.Strict
import Control.Monad.Reader
import Data.Text
import Data.Maybe
import Data.Aeson
import Network.EngineIO (SocketId)
import Network.SocketIO
import Data.ByteString.Char8
import Linear.V2
import Linear.V
import Data.Text.Encoding
import Character
import GameEngine
import Reactive.Banana.Combinators
import Control.Concurrent
import Data.Time
import UserManagement (PlayerNames)

type Move = V2 Float
type Direction = V2 Float

--TODO move to helper
toId :: ByteString -> Id
toId = decodeUtf8

data UserInput = Movement Move Socket | Shoot Direction Socket | Both Move Direction Socket Socket
instance GetSocket UserInput where
    getSocket (Movement _ s) = s
    getSocket (Shoot _ s) = s
    getSocket (Both _ _ s _) = s


getSocketBehavior :: Behavior PlayerNames -> Behavior Socket
getSocketBehavior players = (\ ma -> Prelude.head $ Map.keys ma) <$> players

fpsClock :: Behavior PlayerNames -> MomentIO((Event UTCTime,Behavior Socket))
fpsClock playerNames = do
                        fpsEvent <- fps 30
                        return (fpsEvent, getSocketBehavior playerNames)

gameStateManager :: (MonadIO m, MonadState RoutingTable m) => m (MomentIO (Behavior GameEngine))
gameStateManager  = do
    userInputSocket <- createSocketEvent "userInput"
    userShootSocket <- createSocketEvent "userShoot"
    return $ do
        userInput  <-  userInputSocket
        userShoot <- userShootSocket
        fpsEventMoment <- fps 30

        let eUpdate = (updateWorld) <$ fpsEventMoment
        --let eSend = (notifyMove) <$ fpsEventMoment
        bWorld <- accumB getNewGameState eUpdate
        let inputEvent = (\(s, n) -> Movement n s) <$> userInput
        let shootEvent = (\(s, n) -> Shoot n s) <$> userShoot
        stepper getNewGameState (bWorld <@  (fmap input  $ unionWith (\ (Movement n s) (Shoot d s') -> Both n d s s') inputEvent shootEvent))
    where
        input (Movement n s) m = handleControlV2 m n (toId (socketId s))
        input (Shoot d s) m = handleShoot d (toId (socketId s)) m
        input (Both n d s s') m = handleShoot d (toId (socketId s')) $ handleControlV2 m n (toId (socketId s))
        input x m = m

updateWorld :: GameEngine -> GameEngine
updateWorld m = updateBullets m

--Dont really need it right now
notifyMove :: GameEngine -> UTCTime -> EventHandler()
notifyMove n time = broadcastAll "updateGameState" (getGameStateForJSON n)

gameStateSender ::GameEngine -> Socket -> UTCTime -> EventHandler()
gameStateSender game sock time = notifyMove game time

gameStateSenderTest x _ = broadcast "updateGameState" (getGameStateForJSON x)
