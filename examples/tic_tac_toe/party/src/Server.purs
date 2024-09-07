module Server where

import Prelude

import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import PartyKit.Server (Connection, PartyServer, create, room)
import PartyKit.Server.Room (storage)
import PartyKit.Server.Room as Room

main :: Effect PartyServer
main = create
  { constructor: \_ -> pure unit
  , onConnect: \_ _ _ -> Console.log "Connecting"
  , onMessage
  , onStart
  }

onStart :: PartyServer -> Aff Unit
onStart server = do
  liftEffect $ Console.log "Starting Server"
  Room.put (storage $ room server) "wow" 0
  pure unit

onMessage :: PartyServer -> Either String ArrayBuffer -> Connection -> Aff Unit
onMessage server msg conn = do
  case msg of
    Left s -> liftEffect $ Console.log $ "Text data " <> s
    Right _ -> pure unit
