module Data.User
  ( Username(..)
  , Email(..)
  , Password(..)
  , Profile(..)
  , ProfileBase(..)
  , User(..)
  , Image
  , Token
  , storeUser
  , retrieveUser
  , fromImage
  ) where

import Prelude

import Data.Argonaut as A
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect (Effect)
import Web.HTML as DOM
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem)

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
    , image :: Image
    | r
    }

newtype Token
  = Token String

derive newtype instance encodeJsonToken :: A.EncodeJson Token

derive newtype instance decodeJsonToken :: A.DecodeJson Token

newtype Image
  = Image (Maybe String)

derive newtype instance encodeJsonImage :: A.EncodeJson Image

derive newtype instance decodeJsonImage :: A.DecodeJson Image

fromImage :: Image -> String
fromImage (Image (Just url)) = url
fromImage (Image Nothing) = "https://static.productionready.io/images/smiley-cyrus.jpg"

userKey :: String
userKey = "conduit-user"

storeUser :: User -> Effect Unit
storeUser userValue = do
  window <- DOM.window
  storage <- localStorage window
  setItem userKey (A.stringify $ A.encodeJson userValue) storage

retrieveUser :: Effect (Maybe User)
retrieveUser = do
  window <- DOM.window
  storage <- localStorage window
  storedValue <- getItem userKey storage
  let 
    decoded = 
      do  rawUser <- storedValue
          jsonUser <- hush $ A.jsonParser rawUser 
          hush $ A.decodeJson jsonUser
  pure decoded
