module Data.ButtonStatus where

import Prelude
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

data ButtonStatus
  = Active
  | Inactive

derive instance eqButtonStatus :: Eq ButtonStatus

disabled :: forall r i. ButtonStatus -> HH.IProp ( disabled :: Boolean | r ) i
disabled status = HP.disabled $ status == Inactive
