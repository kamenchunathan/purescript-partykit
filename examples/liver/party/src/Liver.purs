module Liver (server) where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import PartyServer (Server, create, broadcast)
import PartyServer.Connection (Connection, ConnectionContext)
import PartyServer.Connection as Connection
import Simple.JSON (readJSON, writeJSON)
import Types (ClientMessage(..), ServerMessage(..))

data State = LiveViewsers Int

server :: forall s. Effect (Server s)
server = pure $ create { onMessage, onConnect  }

onConnect :: forall s. Server s -> Connection s -> ConnectionContext -> Aff Unit
onConnect _ conn _ = 
  liftEffect $ Connection.send conn (Connection.id conn)

onMessage :: forall s. Server s -> Connection s -> String -> Aff Unit
onMessage srv conn msg = do
  case readJSON msg of
    Right (SyncMousePosition pos) -> do
      let connId = Connection.id conn
      liftEffect $ broadcast srv
        (writeJSON (BroadcastMousePosition { id: connId, x: pos.x, y: pos.y }))
        []
    _ -> pure unit

