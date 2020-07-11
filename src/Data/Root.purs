module Data.Root (Root(..), Port(..)) where

import Prelude
import Data.Argonaut as A
import Data.Newtype (class Newtype)

newtype Port
  = Port Int

derive instance newtypePort :: Newtype Port _

derive newtype instance encodeJsonPort :: A.EncodeJson Port

derive newtype instance decodeJsonPort :: A.DecodeJson Port

derive newtype instance showPort :: Show Port

data Root
  = PublicApi
  | LocalHost Port
  | CustomBackend String

instance showRoot :: Show Root where
  show PublicApi = "https://conduit.productionready.io/api/"
  show (LocalHost port) = "http://localhost:" <> show port <> "/api/"
  show (CustomBackend url) = url
