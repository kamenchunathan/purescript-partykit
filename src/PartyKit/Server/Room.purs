module PartyKit.Server.Room
  ( Context
  , Room
  , Storage
  , ai
  , broadcast
  , context
  , delete
  , deleteAll
  , env
  , get
  , id
  , internalID
  , parties
  , put
  , storage
  , vectorize
  )
  where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFn1, EffectFn2)
import Effect.Uncurried (EffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)
import Foreign (Foreign)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Room ∷ Type

foreign import data Context ∷ Type

foreign import data Storage ∷ Type

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
foreign import broadcast :: Room -> String -> Array String -> Effect Unit

parties :: Context -> Foreign 
parties = unsafeCoerce >>> _.uri

foreign import getImpl :: forall @a. EffectFn2 Storage String  (Promise (Nullable a))

get :: forall a. Storage -> String -> Aff (Maybe a)
get s k = (runEffectFn2 getImpl s k) # toAffE <#> toMaybe

foreign import putImpl :: forall a. EffectFn3 Storage  String  a  (Promise Unit)

put :: forall a. Storage -> String -> a -> Aff Unit
put s k v = (runEffectFn3 putImpl s k v) # toAffE

foreign import deleteImpl :: EffectFn2 Storage  String  (Promise Boolean)

delete :: Storage -> String -> Aff Boolean
delete s k = runEffectFn2 deleteImpl s k # toAffE

foreign import deleteAllImpl :: EffectFn1 Storage (Promise Unit)

deleteAll :: Storage -> Aff Unit
deleteAll s = runEffectFn1 deleteAllImpl s # toAffE


