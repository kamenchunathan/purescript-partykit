module PartyKit.PartySocket
  ( PartySocket
  , PartySocketArgs
  , createPartySocket
  , onClose
  , onError
  , onMessage
  , onOpen
  , sendArrayBuffer
  , sendString
  , toEventTarget
  ) where

import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toNullable)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (EventTarget)

foreign import data PartySocket ∷ Type

type PartySocketArgs =
  { host :: String
  , room :: String
  , id :: Maybe String
  , party :: Maybe String
  }

type PartySocketImplArgs =
  { host :: String
  , room :: String
  , id :: Nullable String
  , party :: Nullable String
  }

foreign import createPartySocketImpl ∷ EffectFn1 PartySocketImplArgs PartySocket

createPartySocket :: PartySocketArgs -> Effect PartySocket
createPartySocket args = runEffectFn1 createPartySocketImpl implArgs
  where
  implArgs =
    { host: args.host
    , room: args.room
    , id: toNullable args.id
    , party: toNullable args.party
    }

toEventTarget :: PartySocket -> EventTarget
toEventTarget = unsafeCoerce

onOpen :: EventType
onOpen = EventType "open"

onMessage :: EventType
onMessage = EventType "message"

onError :: EventType
onError = EventType "error"

onClose :: EventType
onClose = EventType "close"

foreign import sendImpl :: forall a. EffectFn2 PartySocket a Unit

sendString :: PartySocket -> String -> Effect Unit
sendString = runEffectFn2 sendImpl

sendArrayBuffer :: PartySocket -> ArrayBuffer -> Effect Unit
sendArrayBuffer = runEffectFn2 sendImpl

