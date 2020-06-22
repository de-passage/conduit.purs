module Pages.DevTools (Output(..), Slot, Query(..), component) where

import Prelude
import API.Url (UrlRepository)
import Data.Const (Const)
import Data.Root (Root)
import Halogen as H
import Halogen.HTML as HH

type Input
  = { urls :: UrlRepository }

data Output
  = RootChanged Root

type Query
  = Const Void

type Slot
  = H.Slot Query Output

data Action
  = ChangeRoot Root
  | ChangeCustomRootText String

type State
  = { urls :: UrlRepository
    , customRootText :: String
    }

type ChildSlots
  = ()

component :: forall q m. H.Component HH.HTML q Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
    }
  where
  initialState :: Input -> State
  initialState { urls } = { urls, customRootText: "" }

  render :: State -> H.ComponentHTML Action () m
  render _ = HH.div_ [ HH.text "Hello dev tools!" ]
