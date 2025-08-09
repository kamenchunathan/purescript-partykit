module App where

import Prelude

import Data.Array as Array
import Data.Either (hush)
import Data.Map (Map)
import Data.Map as Map
import Data.Array ((!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.Tuple (Tuple(..))
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
  , selfId :: Maybe String
  }

initialState :: forall input. input -> State
initialState _ = { partySocket: Nothing, positions: Map.empty, selfId: Nothing }

data Action
  = Connect
  | HandleMessage (Maybe ServerMessage)
  | UpdateMousePosition MouseEvent

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Connect -> do
    st <- H.get
    case st.partySocket of 
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
        H.put st { partySocket = Just partySocket, selfId = Nothing , positions = st.positions }
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

colorForId :: String -> String
colorForId id =
  let
    colors = ["bg-red-500", "bg-blue-500", "bg-green-500", "bg-yellow-500", "bg-purple-500", "bg-pink-500"]
    index = String.length id `mod` Array.length colors
  in
    fromMaybe "bg-gray-500" (colors !! index)

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  HH.main
    [ HP.class_ $ ClassName "bg-gray-50 text-gray-800 min-h-screen font-sans" ]
    [ HH.div
        [ HE.onMouseMove UpdateMousePosition
        , HP.classes [ClassName "relative w-full h-screen cursor-none"]
        ]
        ( [ HH.div
              [ HP.classes [ClassName "absolute top-0 left-0 right-0 p-4 text-center"] ]
              [ HH.h1
                  [ HP.classes [ClassName "text-3xl font-bold"] ]
                  [ HH.text "PureScript ❤️ PartyKit" ]
              , HH.p
                  [ HP.classes [ClassName "text-lg text-gray-600"] ]
                  [ HH.text "Real-time cursors with Cloudflare's PartyServer" ]
              ]
          ]
            <> ( Map.toUnfoldable state.positions <#> \(Tuple id pos) ->
                  let
                    colorClass = colorForId id
                    isSelf = state.selfId == Just id
                    label = if isSelf then "You" else String.take 4 id
                  in
                    HH.div
                      [ HP.classes [ClassName "absolute flex items-center space-x-2 pointer-events-none"]
                      , HP.style ("left: " <> show pos.x <> "px; top: " <> show pos.y <> "px;")
                      ]
                      [ HH.div -- cursor
                          [ HP.classes [ClassName "w-4 h-4 rounded-full", ClassName colorClass] ]
                          []
                      , HH.div -- label
                          [ HP.classes [ClassName "px-2 py-1 text-sm text-white rounded-md", ClassName colorClass] ]
                          [ HH.text label ]
                      ]
               )
        )
    ]

component :: forall q o m. MonadAff m => H.Component q Unit o m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Connect
      }
  }
