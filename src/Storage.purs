module Storage
  ( deleteStoredUser
  , storeUser
  , retrieveUser
  , retrieveRepository
  , saveRepository
  ) where

import Prelude
import API.Url (UrlRepository)
import API.Url as Urls
import Data.Argonaut as A
import Data.Either (hush)
import Data.Maybe (Maybe)
import Data.Root (Root(..))
import Data.User (User)
import Effect (Effect)
import Web.HTML (window) as DOM
import Web.HTML.Window (localStorage) as DOM
import Web.Storage.Storage (Storage, getItem, removeItem, setItem)

userKey :: String
userKey = "conduit-user"

urlRepoKey :: String
urlRepoKey = "conduit-url-repository"

localStorage :: Effect Storage
localStorage = do
  window <- DOM.window
  DOM.localStorage window

storeUser :: User -> Effect Unit
storeUser userValue = do
  storage <- localStorage
  setItem userKey (A.stringify $ A.encodeJson userValue) storage

retrieveUser :: Effect (Maybe User)
retrieveUser = do
  storage <- localStorage
  storedValue <- getItem userKey storage
  let
    decoded = do
      rawUser <- storedValue
      jsonUser <- hush $ A.jsonParser rawUser
      hush $ A.decodeJson jsonUser
  pure decoded

deleteStoredUser :: Effect Unit
deleteStoredUser = localStorage >>= removeItem userKey

retrieveRepository :: Effect UrlRepository
retrieveRepository = do
  storage <- localStorage
  value <- getItem urlRepoKey storage
  pure $ Urls.repository PublicApi

saveRepository :: UrlRepository -> Effect Unit
saveRepository repo = do
  pure unit
