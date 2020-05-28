module Main where

import Prelude

import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)

type State
  = Unit

data Action
  = Unit

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body

component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState = const unit

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_ [ HH.text "Hello Conduit!" ]

handleAction ∷ forall o m. Action → H.HalogenM State Action () o m Unit
handleAction _ = pure unit
