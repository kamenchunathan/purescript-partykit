module PartyServer.Connection
  ( Connection
  , ConnectionContext
  , request
  , id
  , server
  , state
  , setStateValue
  , setStateFn
  , send
  ) where

import Prelude

import Data.Function.Uncurried (Fn1, mkFn1)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toNullable)
import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Fetch.Core.Request (Request)

foreign import data Connection ∷ Type -> Type
foreign import data ConnectionContext ∷ Type

foreign import request :: ConnectionContext -> Request

foreign import id :: forall s. Connection s -> String
foreign import server :: forall s. Connection s -> String
foreign import state :: forall s. Connection s -> s

foreign import setStateValueImpl :: forall s. EffectFn2 (Connection s) (Nullable s) s
setStateValue :: forall s. Connection s -> Maybe s -> Effect s
setStateValue conn s = runEffectFn2 setStateValueImpl conn (toNullable s)

foreign import setStateFnImpl :: forall s. EffectFn2 (Connection s) (Fn1 s s) s
setStateFn :: forall s. Connection s -> (s -> s) -> Effect s
setStateFn conn f = runEffectFn2 setStateFnImpl conn (mkFn1 f)

foreign import sendImpl :: forall s. EffectFn2 (Connection s) String Unit

send :: forall s. Connection s -> String -> Effect Unit
send conn msg = runEffectFn2 sendImpl conn msg


