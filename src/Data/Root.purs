module Data.Root (Root(..)) where

import Prelude

data Root
  = PublicApi
  | LocalHost Int
  | CustomBackend String

instance showRoot :: Show Root where
  show PublicApi = "https://conduit.productionready.io/api/"
  show (LocalHost port) = "http://localhost:" <> show port <> "/api/"
  show (CustomBackend url) = url
