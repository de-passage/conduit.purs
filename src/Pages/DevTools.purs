module Pages.DevTools (Output(..), Slot, Query(..)) where

import Prelude
import Data.Const (Const)
import Data.Root (Root)
import Halogen as H

type Input
  = ()

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
  = { root :: String
    , customRootText :: String
    }
