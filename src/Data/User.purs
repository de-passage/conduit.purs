module Data.User
  ( Username(..)
  , Email
  , Password
  , Profile(..)
  , ProfileBase(..)
  , User(..)
  , Image
  , module Token
  , fromImage
  , toMaybe
  ) where

import Prelude
import Data.Argonaut as A
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Token (Token) as Token

newtype Username
  = Username String

newtype Email
  = Email String

newtype Password
  = Password String

derive instance newtypeUsername :: Newtype Username _

derive instance eqUsername :: Eq Username

derive instance newtypeEmail :: Newtype Email _

derive instance newtypePassword :: Newtype Password _

derive newtype instance encodeJsonUsername :: A.EncodeJson Username

derive newtype instance decodeJsonUsername :: A.DecodeJson Username

derive newtype instance encodeJsonEmail :: A.EncodeJson Email

derive newtype instance decodeJsonEmail :: A.DecodeJson Email

derive newtype instance encodeJsonPassword :: A.EncodeJson Password

derive newtype instance decodeJsonPassword :: A.DecodeJson Password

instance showEmail :: Show Email where
  show = unwrap

instance showPassword :: Show Password where
  show = unwrap

instance showUsername :: Show Username where
  show = unwrap

type User
  = ProfileBase
      ( email :: Email
      , token :: Token.Token
      )

type Profile
  = ProfileBase ( following :: Boolean )

type ProfileBase r
  = { username :: Username
    , bio :: Maybe String
    , image :: Image
    | r
    }

newtype Image
  = Image (Maybe String)

derive newtype instance encodeJsonImage :: A.EncodeJson Image

derive newtype instance decodeJsonImage :: A.DecodeJson Image

instance showImage :: Show Image where
  show (Image (Just str)) = str
  show (Image Nothing) = ""

fromImage :: Image -> String
fromImage (Image (Just url)) = url

fromImage (Image Nothing) = "https://static.productionready.io/images/smiley-cyrus.jpg"

toMaybe :: Image -> Maybe String
toMaybe (Image i) = i
