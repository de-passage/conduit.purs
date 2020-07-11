module Data.Root (Root(..), Port(..), port, rootUrl) where

import Prelude
import Data.Argonaut as A
import Data.Maybe (Maybe(..))
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
  show (LocalHost p) = "http://localhost:" <> show p <> "/api/"
  show (CustomBackend url) = url

port :: Root -> Maybe Port
port (LocalHost p) = Just p

port _ = Nothing

rootUrl :: Root -> Maybe String
rootUrl (CustomBackend u) = Just u

rootUrl _ = Nothing
