module PartyKit.Server
  ( ConnectionContext
  , ExecutionContext
  , Lobby
  , PartyServer
  , Request
  , create
  , mkEffectMethod1
  , mkEffectMethod2
  , mkEffectMethod3
  , room
  , module PartyKit.Server.Connection
  , module PartyKit.Server.Room
  ) where

import Prelude

import Control.Promise (Promise, fromAff)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, Error)
import Effect.Uncurried
  ( EffectFn1
  , EffectFn2
  , EffectFn3
  , mkEffectFn1
  , mkEffectFn3
  , runEffectFn1
  )
import Option as Option
import Prim.Row (class Union)
import Record as Record
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import PartyKit.Server.Connection (Connection)
import PartyKit.Server.Room (Room)

foreign import data ConnectionContext ∷ Type
foreign import data Request ∷ Type
foreign import data Lobby ∷ Type
foreign import data ExecutionContext ∷ Type

foreign import data PartyServer ∷ Type

type RequiredArgs = (constructor :: Room -> Effect Unit)
type RequiredArgsImpl = (constructor :: EffectFn1 Room Unit)

type OptionalArgs =
  ( onConnect :: PartyServer -> Connection -> ConnectionContext -> Effect Unit
  , onStart :: PartyServer -> Aff Unit
  , onMessage :: PartyServer -> (Either String ArrayBuffer) -> Connection -> Aff Unit
  , onClose :: PartyServer -> Connection -> Aff Unit
  , onError :: PartyServer -> Connection -> Error -> Aff Unit
  , onRequest :: PartyServer -> Request -> Aff Unit
  , onAlarm :: PartyServer -> Aff Unit
  , onBeforeRequest :: Request -> Lobby -> ExecutionContext -> Aff Request
  , onBeforeConnect :: Request -> Lobby -> ExecutionContext -> Aff Request
  )

type OptionalArgsImpl =
  ( onConnect :: EffectFn2 Connection ConnectionContext Unit
  , onStart :: Effect (Promise Unit)
  -- The void argument could either be a string or arraybuffer
  , onMessage :: EffectFn2 Void Connection (Promise Unit)
  , onClose :: EffectFn1 Connection (Promise Unit)
  , onError :: EffectFn2 Connection Error (Promise Unit)
  , onRequest :: EffectFn1 Request (Promise Unit)
  , onAlarm :: Effect (Promise Unit)
  , onBeforeRequest :: EffectFn3 Request Lobby ExecutionContext (Promise Request)
  , onBeforeConnect :: EffectFn3 Request Lobby ExecutionContext (Promise Request)
  )

foreign import eitherImpl :: forall a b c d. (a -> Either a b) -> (b -> Either a b) -> (Either a b -> c) -> d -> c

foreign import createImpl :: ∀ r. EffectFn1 r PartyServer

-- Necessary to add union constraint
createImpl' :: ∀ r. Union RequiredArgsImpl OptionalArgsImpl r => EffectFn1 (Option.Option r) PartyServer
createImpl' = createImpl

create ∷ ∀ r. Option.FromRecord r RequiredArgs OptionalArgs => Record r -> Effect PartyServer
create args =
  runEffectFn1 createImpl' (Option.insert' required optional)
  where
  recordArgs :: Option.Record RequiredArgs OptionalArgs
  recordArgs = Option.recordFromRecord args

  required =
    recordArgs
      # Option.required
      # Record.modify (Proxy @"constructor") mkEffectFn1

  optional =
    recordArgs
      # Option.optional
      # Option.modify (Proxy @"onConnect") mkEffectMethod3
      # Option.modify (Proxy @"onStart") (mkEffectMethod1 <<< (<<<) fromAff)
      # Option.modify (Proxy @"onMessage")
          ( \f -> mkEffectMethod3
              (\a b c -> fromAff $ (flip $ (eitherImpl Left Right <<< flip) f) a b c)
          )
      # Option.modify (Proxy @"onClose") (\f -> mkEffectMethod2 (\a b -> fromAff $ f a b))
      # Option.modify (Proxy @"onError") (\f -> mkEffectMethod3 (\a b c -> fromAff $ f a b c))
      # Option.modify (Proxy @"onRequest") (\f -> mkEffectMethod2 (\a b -> fromAff $ f a b))
      # Option.modify (Proxy @"onAlarm") (mkEffectMethod1 <<< (<<<) fromAff)
      # Option.modify (Proxy @"onBeforeRequest") (\f -> mkEffectFn3 (\a b c -> fromAff $ f a b c))
      # Option.modify (Proxy @"onBeforeConnect") (\f -> mkEffectFn3 (\a b c -> fromAff $ f a b c))

foreign import mkEffectMethod1 :: forall this r. (this -> Effect r) -> Effect r

foreign import mkEffectMethod2 :: forall this a r. (this -> a -> Effect r) -> EffectFn1 a r

foreign import mkEffectMethod3 :: forall this a b r. (this -> a -> b -> Effect r) -> EffectFn2 a b r

room :: PartyServer -> Room
room = unsafeCoerce >>> _.room

