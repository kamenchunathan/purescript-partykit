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
import Data.Nullable as Nullable
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
import Type.Proxy (Proxy(..))
import PartyKit.Server.Connection (Connection)
import PartyKit.Server.Room (Room)
import Prim.Row (class Union)
import Record as Record
import Unsafe.Coerce (unsafeCoerce)

foreign import data ConnectionContext ∷ Type
foreign import data Request ∷ Type
foreign import data Lobby ∷ Type
foreign import data ExecutionContext ∷ Type

foreign import data PartyServer ∷ Type

type RequiredArgs = (constructor :: Room -> Effect Unit)

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

type ArgsImpl =
  ( 
  constructor :: Nullable.Nullable ( EffectFn1 Room Unit)
  ,  onConnect :: Nullable.Nullable ( EffectFn2 Connection ConnectionContext Unit)
  , onStart :: Nullable.Nullable ( Effect (Promise Unit))
  -- The void argument could either be a string or arraybuffer
  , onMessage :: Nullable.Nullable ( EffectFn2 Void Connection (Promise Unit))
  , onClose :: Nullable.Nullable ( EffectFn1 Connection (Promise Unit))
  , onError :: Nullable.Nullable ( EffectFn2 Connection Error (Promise Unit))
  , onRequest :: Nullable.Nullable ( EffectFn1 Request (Promise Unit))
  , onAlarm :: Nullable.Nullable ( Effect (Promise Unit))
  , onBeforeRequest :: Nullable.Nullable ( EffectFn3 Request Lobby ExecutionContext (Promise Request))
  , onBeforeConnect :: Nullable.Nullable ( EffectFn3 Request Lobby ExecutionContext (Promise Request))
  )

foreign import eitherImpl
  :: forall a b c d
   . (a -> Either a b)
  -> (b -> Either a b)
  -> (Either a b -> c)
  -> d
  -> c

foreign import createImpl :: ∀ r. EffectFn1 r PartyServer

create
  ∷ ∀ args r complete
   . Union args r OptionalArgs
  => Union RequiredArgs args complete
  => Record complete
  -> Effect PartyServer
create args =
  runEffectFn1 createImpl argsImpl
  where
  recordArgs
    :: { constructor :: Nullable.Nullable (Room -> Effect Unit)
       , onConnect :: Nullable.Nullable (PartyServer -> Connection -> ConnectionContext -> Effect Unit)
       , onStart :: Nullable.Nullable (PartyServer -> Aff Unit)
       , onMessage :: Nullable.Nullable (PartyServer -> (Either String ArrayBuffer) -> Connection -> Aff Unit)
       , onClose :: Nullable.Nullable (PartyServer -> Connection -> Aff Unit)
       , onError :: Nullable.Nullable (PartyServer -> Connection -> Error -> Aff Unit)
       , onRequest :: Nullable.Nullable (PartyServer -> Request -> Aff Unit)
       , onAlarm :: Nullable.Nullable (PartyServer -> Aff Unit)
       , onBeforeRequest :: Nullable.Nullable (Request -> Lobby -> ExecutionContext -> Aff Request)
       , onBeforeConnect :: Nullable.Nullable (Request -> Lobby -> ExecutionContext -> Aff Request)
       }
  recordArgs = unsafeCoerce args

  mapNullable :: forall a b. (a -> b) -> Nullable.Nullable a -> Nullable.Nullable b
  mapNullable f = Nullable.toMaybe >>> (map f) >>> Nullable.toNullable

  argsImpl :: Record ArgsImpl
  argsImpl =
    recordArgs
      # Record.modify (Proxy @"constructor") (mapNullable mkEffectFn1)
      # Record.modify (Proxy @"onConnect") (mapNullable mkEffectMethod3)
      # Record.modify (Proxy @"onStart") (mapNullable (mkEffectMethod1 <<< (<<<) fromAff))
      # Record.modify (Proxy @"onMessage")
          ( mapNullable
              ( \f -> mkEffectMethod3
                  (\a b c -> fromAff $ (flip $ (eitherImpl Left Right <<< flip) f) a b c)
              )
          )
      # Record.modify (Proxy @"onClose") (mapNullable (\f -> mkEffectMethod2 (\a b -> fromAff $ f a b)))
      # Record.modify (Proxy @"onError") (mapNullable (\f -> mkEffectMethod3 (\a b c -> fromAff $ f a b c)))
      # Record.modify (Proxy @"onRequest") (mapNullable (\f -> mkEffectMethod2 (\a b -> fromAff $ f a b)))
      # Record.modify (Proxy @"onAlarm") (mapNullable (mkEffectMethod1 <<< (<<<) fromAff))
      # Record.modify (Proxy @"onBeforeRequest") (mapNullable (\f -> mkEffectFn3 (\a b c -> fromAff $ f a b c)))
      # Record.modify (Proxy @"onBeforeConnect") (mapNullable (\f -> mkEffectFn3 (\a b c -> fromAff $ f a b c)))

foreign import mkEffectMethod1 :: forall this r. (this -> Effect r) -> Effect r

foreign import mkEffectMethod2 :: forall this a r. (this -> a -> Effect r) -> EffectFn1 a r

foreign import mkEffectMethod3 :: forall this a b r. (this -> a -> b -> Effect r) -> EffectFn2 a b r

-- foreign import room ∷ PartyServer -> Room

room :: PartyServer -> Room
room = unsafeCoerce >>> _.room

