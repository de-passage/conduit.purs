module Data.Token (Token, authorizationHeader) where

import Prelude (($), (<>))
import Affjax.RequestHeader as AJRH
import Data.Argonaut as A

newtype Token
  = Token String

derive newtype instance encodeJsonToken :: A.EncodeJson Token

derive newtype instance decodeJsonToken :: A.DecodeJson Token

authorizationHeader :: Token -> AJRH.RequestHeader
authorizationHeader (Token token) = AJRH.RequestHeader "Authorization" $ "Token " <> token