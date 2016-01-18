{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Example (server, ServerState (..)) where

import Prelude hiding (mapM_)

import Control.Monad.IO.Class (liftIO)
import Control.Applicative
import Data.Foldable (mapM_)

import qualified Control.Concurrent.STM as STM
import qualified Data.Text as Text
import qualified Network.SocketIO as SocketIO

data ServerState = ServerState { ssNConnected :: STM.TVar Int }

--server :: ServerState -> StateT SocketIO.RoutingTable Snap.Snap ()
server state = do
  userNameMVar <- liftIO STM.newEmptyTMVarIO
  let forUserName m = liftIO (STM.atomically (STM.tryReadTMVar userNameMVar)) >>= mapM_ m
  SocketIO.on "example" $ \ example  ->
    forUserName $ \userName -> SocketIO.emit "example" $ (Text.pack $ show ((example::Integer) + 1))
