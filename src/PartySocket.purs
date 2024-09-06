module PartySocket
  ( createPartySocket
  , PartySocket
  , PartySocketArgs
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toNullable)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, mkEffectFn1, runEffectFn1)
import Web.Event.Event (Event)

foreign import data PartySocket ∷ Type

type PartySocketArgs =
  { host :: String
  , room :: String
  , id :: Maybe String
  , party :: Maybe String
  , onMessage :: Maybe ( Event -> Effect Unit)
  }

type PartySocketImplArgs =
  { host :: String
  , room :: String
  , id :: Nullable String
  , party :: Nullable String
  , onMessage :: Nullable (EffectFn1 Event Unit)
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
    , onMessage : toNullable $ mkEffectFn1 <$> args.onMessage
    }
