module Data.Root (Root(..), Port) where

import Prelude

newtype Port
  = Port Int

instance showPort :: Show Port where
  show (Port s) = show s

data Root
  = PublicApi
  | LocalHost Port
  | Custom String
