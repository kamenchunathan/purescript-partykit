module Types where

import Prelude

import Control.Monad.Except (except)
import Foreign (ForeignError(..), fail)
import Simple.JSON (class ReadForeign, class WriteForeign, read, write)

newtype Counter = Counter Int

derive instance Eq Counter
derive instance Ord Counter
derive newtype instance ReadForeign Counter
derive newtype instance WriteForeign Counter

data ClientMessage = Increment | Decrement

instance WriteForeign ClientMessage where
  writeImpl = case _ of
    Increment -> write "Increment"
    Decrement -> write "Decrement"

instance ReadForeign ClientMessage where
  readImpl value = do
    str <- except $ read value
    case str of
      "Increment" -> pure Increment
      "Decrement" -> pure Decrement
      _ -> fail $ ForeignError "Could not decode ClientMessage"

data ServerMessage = UpdateCount Counter

instance WriteForeign ServerMessage where
  writeImpl = case _ of
    UpdateCount count -> write { count: write count, tag: "UpdateCount" }

instance ReadForeign ServerMessage where
  readImpl value = do
    obj :: { count :: Int, tag :: String } <- except $ read value
    case obj.tag of
      "UpdateCount" -> pure $ UpdateCount $ Counter obj.count
      _ -> fail $ ForeignError "Could not decode ServerMessage"
