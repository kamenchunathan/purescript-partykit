module App where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Halogen as H
import Halogen.HTML (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PartyKit.PartySocket (PartySocket, createPartySocket, onOpen, sendString, toEventTarget)
import Web.Event.EventTarget (addEventListener, eventListener)

type State =
  { partySocket :: Maybe PartySocket
  }

initialState :: forall input. input -> State
initialState _ = { partySocket: Nothing }

data Action
  = Connect
  | SendRandomMsg

handleAction :: forall o m. MonadEffect m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Connect -> do
    partySocket <- liftEffect do
      partySocket <- createPartySocket
        { host: "localhost:1999"
        , room: "pong"
        , id: Nothing
        , party: Nothing
        }
      onOpenListener <- eventListener \_ -> do
        Console.log "Socket Open"
      onMessageListener <- eventListener \e -> do
        Console.log "Socket Open"
      addEventListener onOpen onOpenListener false (toEventTarget partySocket)
      pure partySocket
    H.put { partySocket: Just partySocket }

  SendRandomMsg -> do
    sock <- H.gets _.partySocket
    case sock of
      Just s -> liftEffect $ sendString s "Random Msg"
      Nothing -> pure unit

render :: forall cs m. State -> H.ComponentHTML Action cs m
render _ =
  HH.div
    [ HP.class_ $ ClassName "w-5/6 mx-auto flex justify-center" ]
    [ HH.button
        [ HP.class_ $ ClassName "px-4 py-2 bg-blue-400 text-white rounded-md shadow"
        , HE.onClick \_ -> Connect
        ]
        [ HH.text "Connect" ]
    , HH.button
        [ HP.class_ $ ClassName "px-4 py-2 bg-blue-400 text-white rounded-md shadow"
        , HE.onClick \_ -> SendRandomMsg
        ]
        [ HH.text "SendRandomMsg" ]

    ]

component :: forall q o m. MonadEffect m => H.Component q Unit o m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction }
  }
