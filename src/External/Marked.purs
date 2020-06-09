module Marked where

import Prelude
import Data.Function.Uncurried (Fn2)
import Effect (Effect)
import Web.HTML (HTMLElement)

foreign import marked :: String -> Effect String

foreign import markedByElementId :: Fn2 String String (Effect Unit)

foreign import setHTML :: HTMLElement -> String -> Effect Unit
