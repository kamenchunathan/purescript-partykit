module PartyServer
  ( Server
  , OptionalArgs
  , RequiredArgs
  , create
  , broadcast
  , getConnection
  , getConnections
  ) where

import Prelude

import Control.Promise (Promise, fromAff)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Effect (Effect)
import Effect.Aff (Aff)
import Fetch.Core.Request (Request)
import Fetch.Core.Response (Response)
import Effect.Uncurried
  ( EffectFn1
  , EffectFn2
  , EffectFn3
  , EffectFn5
  , mkEffectFn1
  , mkEffectFn2
  , mkEffectFn3
  , mkEffectFn5
  , runEffectFn2
  , runEffectFn3
  )
import Foreign (Foreign)
import PartyServer.Connection (Connection, ConnectionContext)
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Server :: Type -> Type


foreign import createImpl :: ∀ s. ArgsImpl s -> Server s

type RequiredArgs :: forall k. Row k
type RequiredArgs = ()

type OptionalArgs s =
  ( onConnect :: Server s -> Connection s -> ConnectionContext -> Aff Unit
  , onStart :: Server s -> Aff Unit
  , onMessage :: Server s -> Connection s -> String -> Aff Unit
  , onClose :: Server s -> Connection s -> Int -> String -> Boolean -> Aff Unit
  , onError :: Server s -> Connection s -> Foreign -> Aff Unit
  , onRequest :: Server s -> Request -> Aff Response
  , onException :: Server s -> Foreign -> Aff Unit
  , onAlarm :: Server s -> Aff Unit
  )

type ArgsImpl s =
  { onConnect :: Nullable (EffectFn3 (Server s) (Connection s) ConnectionContext (Promise Unit))
  , onStart :: Nullable (EffectFn1 (Server s) (Promise Unit))
  , onMessage :: Nullable (EffectFn3 (Server s) (Connection s) String (Promise Unit))
  , onClose :: Nullable (EffectFn5 (Server s) (Connection s) Int String Boolean (Promise Unit))
  , onError :: Nullable (EffectFn3 (Server s) (Connection s) Foreign (Promise Unit))
  , onRequest :: Nullable (EffectFn2 (Server s) Request (Promise Response))
  , onException :: Nullable (EffectFn2 (Server s) Foreign (Promise Unit))
  , onAlarm :: Nullable (EffectFn1 (Server s) (Promise Unit))
  }

create
  ∷ ∀ args r complete s
   . Union args r (OptionalArgs s)
  => Union RequiredArgs args complete
  => Record complete
  -> Server s
create args =
  createImpl $ toArgsImpl $ unsafeCoerce args
  where
  toArgsImpl
    :: { onConnect :: Nullable (Server s -> Connection s -> ConnectionContext -> Aff Unit)
       , onStart :: Nullable (Server s -> Aff Unit)
       , onMessage :: Nullable (Server s -> Connection s -> String -> Aff Unit)
       , onClose :: Nullable (Server s -> Connection s -> Int -> String -> Boolean -> Aff Unit)
       , onError :: Nullable (Server s -> Connection s -> Foreign -> Aff Unit)
       , onRequest :: Nullable (Server s -> Request -> Aff Response)
       , onException :: Nullable (Server s -> Foreign -> Aff Unit)
       , onAlarm :: Nullable (Server s -> Aff Unit)
       }
    -> ArgsImpl s
  toArgsImpl recordArgs =
    { onConnect: recordArgs.onConnect # mapNullable (\f -> mkEffectFn3 (\a b c -> fromAff $ f a b c))
    , onStart: recordArgs.onStart # mapNullable (\f -> mkEffectFn1 (\a -> fromAff $ f a))
    , onMessage: recordArgs.onMessage # mapNullable (\f -> mkEffectFn3 (\a b c -> fromAff $ f a b c))
    , onClose: recordArgs.onClose # mapNullable (\f -> mkEffectFn5 (\a b c d e -> fromAff $ f a b c d e))
    , onError: recordArgs.onError # mapNullable (\f -> mkEffectFn3 (\a b c -> fromAff $ f a b c))
    , onRequest: recordArgs.onRequest # mapNullable (\f -> mkEffectFn2 (\a b -> fromAff $ f a b))
    , onException: recordArgs.onException # mapNullable (\f -> mkEffectFn2 (\a b -> fromAff $ f a b))
    , onAlarm: recordArgs.onAlarm # mapNullable (\f -> mkEffectFn1 (\a -> fromAff $ f a))
    }

  mapNullable :: forall a b. (a -> b) -> Nullable a -> Nullable b
  mapNullable f = toMaybe >>> (map f) >>> toNullable

foreign import broadcastImpl :: forall s. EffectFn3 (Server s) String (Array String) Unit

-- TODO: Add the ability to send types other than string. 
--  Change this to a union of string, Arraybuffer, ArrayBufferView
broadcast :: forall s. Server s -> String -> Array String -> Effect Unit
broadcast server msg without = runEffectFn3 broadcastImpl server msg without 

foreign import getConnectionImpl :: forall s. EffectFn2 (Server s) String (Nullable (Connection s))

getConnection :: forall s. Server s -> String -> Effect (Maybe (Connection s))
getConnection server id = toMaybe <$> runEffectFn2 getConnectionImpl server id

foreign import getConnectionsImpl :: forall s. EffectFn2 (Server s) (Nullable String) (Array (Connection s))

getConnections :: forall s. Server s -> Maybe String -> Effect (Array (Connection s))
getConnections server tag = runEffectFn2 getConnectionsImpl server (toNullable tag)

