module App where

import Prelude

import Data.Array as Array
import Data.Either (hush)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Halogen as H
import Halogen.HTML (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event (eventListener)
import Halogen.Subscription as HS
import PartyKit.PartySocket (PartySocket, createPartySocket, sendString)
import PartyKit.PartySocket as PartySocket
import Simple.JSON (readJSON, writeJSON)
import Types (ClientMessage(..), ServerMessage(..))
import Web.HTML (window)
import Web.HTML.Window (location)
import Web.HTML.Location (host)
import Web.UIEvent.MouseEvent (MouseEvent, clientX, clientY)

type State =
  { partySocket :: Maybe PartySocket
  , positions :: Map String { x :: Int, y :: Int }
  }

initialState :: forall input. input -> State
initialState _ = { partySocket: Nothing,  positions: Map.empty }

data Action
  = Connect
  | HandleMessage (Maybe ServerMessage)
  | UpdateMousePosition MouseEvent

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Connect -> do
    sock <- H.gets _.partySocket
    case sock of
      Just _ -> pure unit
      Nothing -> do
        liftEffect $ Console.log "Starting Application"
        partySocketHost <- liftEffect $ window >>= location >>= host
        partySocket <- H.liftEffect $
          createPartySocket
            { host: partySocketHost
            , room: "counter"
            , id: Nothing
            , party: Just "live"
            }
        void $ H.modify_ _ { partySocket = Just partySocket }
        s <- liftEffect $ partySocketSource partySocket
        void $ H.subscribe s

  HandleMessage (Just (BroadcastMousePosition pos)) -> do
    H.modify_ \st -> st { positions = Map.insert pos.id { x: pos.x, y: pos.y } st.positions }
  HandleMessage _ ->
    pure unit

  UpdateMousePosition event -> do
    sock <- H.gets _.partySocket
    case sock of
      Just s -> do
        let x = clientX event
        let y = clientY event
        liftEffect $ sendString s (writeJSON (SyncMousePosition { x, y }))
      Nothing -> pure unit

partySocketSource :: PartySocket -> Effect (HS.Emitter Action)
partySocketSource sock = do
  Console.log "Hello world"
  pure $ eventListener PartySocket.onMessage (PartySocket.toEventTarget sock) eventToAction
  where
  eventToAction event = PartySocket.fromEvent event
    <#> PartySocket.data_
    <#> (readJSON >>> (map $ HandleMessage <<< Just) >>> hush)
    # join

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  HH.div
    [ HE.onMouseMove UpdateMousePosition
    , HP.style "height: 100vh; width: 100vw; position: relative;"
    ]
    ( (Map.values state.positions) <#> (\{ x, y } ->
        HH.div
          [ HP.class_ (ClassName "cursor")
          , HP.style ("left: " <> show x <> "px; top: " <> show y <> "px;")
          ]
          []
      )
      # Array.fromFoldable
    )

component :: forall q o m. MonadAff m => H.Component q Unit o m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Connect
      }
  }
