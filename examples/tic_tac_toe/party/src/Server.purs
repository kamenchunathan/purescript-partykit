module Server where

import Prelude

import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import PartyKit.Server
  ( Connection
  , ConnectionContext
  , ExecutionContext
  , Lobby
  , PartyServer
  , Request
  , create
  )
import PartyKit.Server as Server
import PartyKit.Server.Connection as Connection
import PartyKit.Server.Room as Room
import PartyKit.Server.Storage as Storage
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

recalculateAndBroadcast :: Server.Room -> Aff Unit
recalculateAndBroadcast room = do
  userOps :: Map String (Tuple Int Int) <- Storage.get (Room.storage room) "ops" <#> fromMaybe Map.empty
  let total = sum $ (\(x /\ y) -> y - x) <$> (Map.values userOps)
  liftEffect $ Room.broadcast room (writeJSON (UpdateCount (Counter total))) []

onConnect :: PartyServer -> Connection -> ConnectionContext -> Effect Unit
onConnect server connection _ = do
  liftEffect $ Console.log $ "Connected: " <> Connection.id connection
  runAff_
    (Console.logShow >>> const (pure unit))
    (recalculateAndBroadcast (Server.room server))

onMessage :: PartyServer -> Either String ArrayBuffer -> Connection -> Aff Unit
onMessage server msg connection = do
  let
    room = Server.room server
    s = Room.storage room
  case msg of
    Left str -> do
      case readJSON str of
        Left e -> liftEffect $ Console.logShow e
        Right message -> do
          userOps :: Map String (Tuple Int Int) <- Storage.get s "ops" <#> (fromMaybe Map.empty)
          let
            updateUserVector (Just (decrs /\ incrs)) = case message of
              Increment -> Just (decrs /\ (incrs + 1))
              Decrement -> Just ((decrs + 1) /\ incrs)
            updateUserVector Nothing = case message of
              Increment -> Just (0 /\ 1)
              Decrement -> Just (1 /\ 0)

            newUserOps = Map.alter updateUserVector (Connection.id connection) userOps
          Storage.put s "ops" newUserOps
          recalculateAndBroadcast room

    Right _ -> pure unit

onBeforeRequest :: Request -> Lobby -> ExecutionContext -> Aff Request
onBeforeRequest req _ _ =
  pure req
