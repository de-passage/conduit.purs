module Data.Tag where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Newtype (class Newtype)

newtype Tag = Tag String

derive instance newtypeTag :: Newtype Tag _
derive instance eqTag :: Eq Tag
derive newtype instance decodeJsonTag :: DecodeJson Tag
derive newtype instance encodeJsonTag :: EncodeJson Tag