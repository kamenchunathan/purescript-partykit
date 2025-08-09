module PartyKit.Server.Room
  ( Context
  , Room
  , ai
  , broadcast
  , context
  , env
  , id
  , internalID
  , parties
  , storage
  , vectorize
  ) where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn3, runEffectFn3)
import Foreign (Foreign)
import Unsafe.Coerce (unsafeCoerce)
import PartyKit.Server.Storage (Storage)

foreign import data Room ∷ Type

foreign import data Context ∷ Type

id :: Room -> String
id = unsafeCoerce >>> _.id

internalID :: Room -> String
internalID = unsafeCoerce >>> _.internalID

env :: Room -> String
env = unsafeCoerce >>> _.env

storage :: Room -> Storage
storage = unsafeCoerce >>> _.storage

context :: Room -> Context
context = unsafeCoerce >>> _.context

ai :: Room -> String
ai = unsafeCoerce >>> _.ai

vectorize :: Room -> String
vectorize = unsafeCoerce >>> _.ai

-- TODO:(nathan) Add ArrayBuffer 
foreign import broadcastImpl :: EffectFn3 Room String (Array String) Unit

broadcast :: Room -> String -> Array String -> Effect Unit
broadcast = runEffectFn3 broadcastImpl

parties :: Context -> Foreign
parties = unsafeCoerce >>> _.uri

