module App where

import Prelude

import Data.Int (decimal, toStringAs)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Halogen as H
import Halogen.HTML (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PartySocket (createPartySocket)

type State = { count :: Int }

initialState :: forall input. input -> State
initialState _ = { count: 0 }

data Action = Connect

handleAction :: forall o m. MonadEffect m => Action -> H.HalogenM State Action () o m Unit
handleAction Connect = do
  partySocket <- liftEffect $ createPartySocket
    { host: "localhost:1999"
    , room: "Wow"
    , id: Nothing
    , party: Nothing
    , onMessage: Just \_ -> do
        Console.log "Message received"
    }
  pure unit

render :: forall cs m. State -> H.ComponentHTML Action cs m
render { count } =
  HH.div
    [ HP.class_ $ ClassName "w-5/6 mx-auto flex justify-center" ]
    [ HH.button
        [ HP.class_ $ ClassName "px-4 py-2 bg-blue-400 text-white rounded-md shadow"
        , HE.onClick \_ -> Connect
        ]
        [ HH.text "Connect" ]
    ]

component :: forall q o m. MonadEffect m => H.Component q Unit o m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction }
  }
