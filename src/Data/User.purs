module Data.User where

import Data.Newtype

import Data.Argonaut as A
import Data.Maybe (Maybe)

newtype Username
  = Username String

newtype Email
  = Email String

newtype Password
  = Password String

derive instance newtypeUsername :: Newtype Username _

derive instance newtypeEmail :: Newtype Email _

derive instance newtypePassword :: Newtype Password _

derive newtype instance encodeJsonUsername :: A.EncodeJson Username

derive newtype instance decodeJsonUsername :: A.DecodeJson Username

derive newtype instance encodeJsonEmail :: A.EncodeJson Email

derive newtype instance decodeJsonEmail :: A.DecodeJson Email

derive newtype instance encodeJsonPassword :: A.EncodeJson Password

derive newtype instance decodeJsonPassword :: A.DecodeJson Password

newtype Token
  = Token String

type User
  = ProfileBase
      ( email :: Email
      , token :: Token
      )

type Profile
  = ProfileBase ( following :: Boolean )

type ProfileBase r
  = { username :: Username
    , bio :: Maybe String
    , image :: String
    | r
    }

derive newtype instance encodeJsonToken :: A.EncodeJson Token

derive newtype instance decodeJsonToken :: A.DecodeJson Token
