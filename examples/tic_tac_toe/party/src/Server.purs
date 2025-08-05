module Server where

import Prelude

import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either(..), hush)
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import PartyKit.Server (Connection, PartyServer, Request, Lobby, ExecutionContext, 
ConnectionContext, create)
import PartyKit.Server as Server
import PartyKit.Server.Room as Room
import Simple.JSON (readJSON, writeJSON)
import Types (ClientMessage(..), Counter(..), ServerMessage(..))

main :: Effect PartyServer
main = create
  { constructor: const $ pure unit
  , onConnect
  , onMessage
  , onStart
  , onBeforeRequest
  }

onStart :: PartyServer -> Aff Unit
onStart _ = do
  liftEffect $ Console.log "Starting Server"
  pure unit

onConnect :: PartyServer -> Connection -> ConnectionContext -> Effect Unit
onConnect server _ _ =  do
  liftEffect $ Console.log "Connected"
  let
    room = Server.room server
    s = Room.storage $ room
  runAff_ 
    (
      hush 
      >>> join
      >>> fromMaybe 0
      <#> \count ->  Room.broadcast room (writeJSON (UpdateCount (Counter count))) []
    ) 
    (Room.get s "count")


onMessage :: PartyServer -> Either String ArrayBuffer -> Connection -> Aff Unit
onMessage server msg _ = do
  let
    room = Server.room server
    s = Room.storage room
  case msg of
    Left str -> do
      case readJSON str of
        Left e -> liftEffect $ Console.logShow e
        Right Increment -> do
          count <- Room.get s "count"
          let
            nextCount = fromMaybe 0 count + 1
          Room.put s "count" nextCount
          liftEffect $ Room.broadcast room (writeJSON (UpdateCount (Counter nextCount))) []
                    
    Right _ -> pure unit

onBeforeRequest :: Request -> Lobby -> ExecutionContext -> Aff Request
onBeforeRequest req _ _ =
  pure req
