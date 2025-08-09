module Types where

import Prelude

import Control.Monad.Except (except)
import Foreign (ForeignError(..), fail)
import Simple.JSON (class ReadForeign, class WriteForeign, read, write)


data ClientMessage = SyncMousePosition { x :: Int, y :: Int }

instance WriteForeign ClientMessage where
  writeImpl = case _ of
    SyncMousePosition pos -> write { tag: "mousePos", x: pos.x, y: pos.y }

instance ReadForeign ClientMessage where
  readImpl value = do
    obj :: { tag :: String } <- except $ read value
    case obj.tag of
      "mousePos" -> do
        posObj :: { x :: Int, y :: Int } <- except $ read value
        pure $ SyncMousePosition { x: posObj.x, y: posObj.y }
      _ -> fail $ ForeignError "Could not decode ClientMessage"

data ServerMessage = BroadcastMousePosition { id :: String, x :: Int, y :: Int }

instance WriteForeign ServerMessage where
  writeImpl = case _ of
    BroadcastMousePosition msg -> write { tag: "broadcastmPos", id: msg.id, x: msg.x, y: msg.y }

instance ReadForeign ServerMessage where
  readImpl value = do
    obj :: { tag :: String } <- except $ read value
    case obj.tag of
      "broadcastmPos" -> do
        posObj :: { id :: String, x :: Int, y :: Int } <- except $ read value
        pure $ BroadcastMousePosition { id: posObj.id, x: posObj.x, y: posObj.y }
      _ -> fail $ ForeignError "Could not decode ServerMessage"
