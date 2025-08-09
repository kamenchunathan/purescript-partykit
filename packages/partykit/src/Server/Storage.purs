module PartyKit.Server.Storage (get, put, delete, deleteAll, Storage) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFn1, EffectFn2)
import Effect.Uncurried (EffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)

foreign import data Storage ∷ Type

foreign import getImpl :: forall @a. EffectFn2 Storage String (Promise (Nullable a))

get :: forall a. Storage -> String -> Aff (Maybe a)
get s k = (runEffectFn2 getImpl s k) # toAffE <#> toMaybe

foreign import putImpl :: forall a. EffectFn3 Storage String a (Promise Unit)

put :: forall a. Storage -> String -> a -> Aff Unit
put s k v = (runEffectFn3 putImpl s k v) # toAffE

foreign import deleteImpl :: EffectFn2 Storage String (Promise Boolean)

delete :: Storage -> String -> Aff Boolean
delete s k = runEffectFn2 deleteImpl s k # toAffE

foreign import deleteAllImpl :: EffectFn1 Storage (Promise Unit)

deleteAll :: Storage -> Aff Unit
deleteAll s = runEffectFn1 deleteAllImpl s # toAffE
