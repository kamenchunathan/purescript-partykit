module App where

import Prelude

import Data.Either (hush)
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
import Simple.JSON (writeJSON, readJSON)
import Types (ClientMessage(..), Counter(..), ServerMessage(..))
import Web.HTML (window)
import Web.HTML.Window (location)
import Web.HTML.Location (host)

type State =
  { partySocket :: Maybe PartySocket
  , count :: Maybe Counter
  }

initialState :: forall input. input -> State
initialState _ = { partySocket: Nothing, count: Nothing }

data Action
  = Connect
  | Incr
  | Decr
  | HandleMessage (Maybe ServerMessage)

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Connect -> do
    sock <- H.gets _.partySocket
    case sock of
      Just _ -> pure unit
      Nothing -> do
        liftEffect $ Console.log "Counter Application"
        partySocketHost <- liftEffect $ window >>= location >>= host
        partySocket <- H.liftEffect $
          createPartySocket
            { host: partySocketHost
            , room: "counter"
            , id: Nothing
            , party: Nothing
            }
        void $ H.modify _ { partySocket = Just partySocket }
        s <- liftEffect $ partySocketSource partySocket
        void $ H.subscribe s -- (partySocketSource partySocket)

  Incr -> do
    sock <- H.gets _.partySocket
    case sock of
      Just s -> liftEffect $ sendString s (writeJSON Increment)
      Nothing -> pure unit

  Decr -> do
    sock <- H.gets _.partySocket
    case sock of
      Just s -> liftEffect $ sendString s (writeJSON Decrement)
      Nothing -> pure unit

  HandleMessage (Just (UpdateCount c)) -> do
    H.modify_ _ { count = Just c }
  HandleMessage _ ->
    pure unit

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
    [ HP.class_ $ ClassName "w-full min-h-screen flex flex-col items-center justify-center space-y-8 p-4 text-center" ]
    [ HH.div [ HP.class_ $ ClassName "space-y-2" ]
        [ HH.h1 [ HP.class_ $ ClassName "text-5xl font-extrabold tracking-tight" ] [ HH.text "Real-time Counter" ]
        , HH.p [ HP.class_ $ ClassName "text-lg text-gray-400" ] [ HH.text "Powered by PureScript, Halogen, and PartyKit." ]
        ]
    , case state.partySocket of
        Nothing ->
          HH.button
            [ HP.class_ $ ClassName "px-8 py-4 bg-blue-600 text-white font-bold rounded-lg shadow-lg hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-blue-500 focus:ring-opacity-75 transition-all duration-200 ease-in-out"
            , HE.onClick \_ -> Connect
            ]
            [ HH.text "Connect to Party" ]
        Just _ ->
          HH.div [ HP.class_ $ ClassName "flex flex-col items-center space-y-6 p-8 bg-gray-800/50 rounded-2xl shadow-2xl w-full max-w-sm" ]
            [ HH.h2 [ HP.class_ $ ClassName "text-2xl font-bold text-gray-300" ] [ HH.text "Current Count" ]
            , HH.p [ HP.class_ $ ClassName "text-8xl font-extrabold text-white tracking-tighter" ]
                [ HH.text $ case state.count of
                    Just (Counter c) -> show c
                    Nothing -> "..."
                ]
            , HH.div [ HP.class_ $ ClassName "flex space-x-4" ]
                [ HH.button
                    [ HP.class_ $ ClassName "px-10 py-5 bg-green-500 text-white font-bold rounded-full shadow-xl hover:bg-green-600 focus:outline-none focus:ring-4 focus:ring-green-400 focus:ring-opacity-50 transform hover:scale-105 transition-all duration-300 ease-in-out"
                    , HE.onClick \_ -> Incr
                    ]
                    [ HH.text "Increment" ]
                , HH.button
                    [ HP.class_ $ ClassName "px-10 py-5 bg-red-500 text-white font-bold rounded-full shadow-xl hover:bg-red-600 focus:outline-none focus:ring-4 focus:ring-red-400 focus:ring-opacity-50 transform hover:scale-105 transition-all duration-300 ease-in-out"
                    , HE.onClick \_ -> Decr
                    ]
                    [ HH.text "Decrement" ]
                ]
            ]
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
