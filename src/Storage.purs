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
import Control.Alt ((<|>))
import Data.Argonaut as A
import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe)
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
  pure $ fromMaybe (Urls.repository PublicApi) (value >>= deserialize)
  where
  deserialize :: String -> Maybe UrlRepository
  deserialize string = do
    json <- hush $ A.jsonParser string
    root <-
      A.caseJsonString Nothing (\s -> if s == "public" then Just PublicApi else Nothing) json
        <|> tryLocalHost json
        <|> tryCustom json
    pure $ Urls.repository root

  tryLocalHost :: A.Json -> Maybe Root
  tryLocalHost json = do
    l <- hush $ A.decodeJson json :: Maybe { localhost :: Int }
    pure $ LocalHost l.localhost

  tryCustom :: A.Json -> Maybe Root
  tryCustom json = do
    c <- hush $ A.decodeJson json :: Maybe { custom :: String }
    pure (CustomBackend c.custom)

saveRepository :: UrlRepository -> Effect Unit
saveRepository repo = do
  storage <- localStorage
  setItem urlRepoKey (serialize repo.root) storage
  where
  public = "public"

  localhost i = { localhost: i }

  custom s = { custom: s }

  serialize :: Root -> String
  serialize PublicApi = public

  serialize (LocalHost port) = A.stringify $ A.encodeJson $ localhost port

  serialize (CustomBackend addr) = A.stringify $ A.encodeJson $ custom addr
