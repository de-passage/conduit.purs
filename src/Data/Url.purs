module Data.Url where

import Data.Newtype (class Newtype)

newtype Url = Url String

derive instance newtypeUrl :: Newtype Url _