module PartyKit.Server.Connection
  ( Connection
  , id
  , send
  , uri
  )
  where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Connection ∷ Type


id :: Connection -> String
id = unsafeCoerce >>> _.id

uri :: Connection -> String
uri = unsafeCoerce >>> _.uri

foreign import sendImpl :: EffectFn2 Connection String Unit

send :: Connection -> String -> Effect Unit
send = runEffectFn2 sendImpl





















