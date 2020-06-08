module Data.Token (Token, authorize) where

import Affjax as AJ
import Affjax.RequestHeader as AJRH
import Data.Argonaut as A
import Data.Array (snoc)
import Prelude (($), (<>))

newtype Token
  = Token String

derive newtype instance encodeJsonToken :: A.EncodeJson Token

derive newtype instance decodeJsonToken :: A.DecodeJson Token

authorizationHeader :: Token -> AJRH.RequestHeader
authorizationHeader (Token token) = AJRH.RequestHeader "Authorization" $ "Token " <> token

authorize :: forall a. AJ.Request a -> Token -> AJ.Request a
authorize r tok = r { headers = r.headers `snoc` authorizationHeader tok }