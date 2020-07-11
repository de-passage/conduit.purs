module Storage
  ( deleteStoredUser
  , storeUser
  , retrieveUser
  , retrieveUrlSettings
  , saveRepository
  , savePerPage
  , retrievePerPage
  , UrlSettings
  ) where

import Prelude
import API.Url (UrlRepository)
import API.Url as Urls
import Control.Alt ((<|>))
import Data.Argonaut as A
import Data.Article (PerPage, perPage)
import Data.Either (hush)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Root (Root(..), Port)
import Data.User (User)
import Effect (Effect)
import Web.HTML (window) as DOM
import Web.HTML.Window (localStorage) as DOM
import Web.Storage.Storage (Storage, getItem, removeItem, setItem)

type UrlSettings
  = { repo :: UrlRepository
    , lastUrl :: Maybe String
    , lastPort :: Maybe Port
    }

userKey :: String
userKey = "conduit-user"

urlRepoKey :: String
urlRepoKey = "conduit-url-repository"

perPageKey :: String
perPageKey = "conduit-url-articles-per-page"

lastUrlKey :: String
lastUrlKey = "conduit-last-url"

lastPortKey :: String
lastPortKey = "conduit-last-port"

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

retrieveUrlSettings :: Effect UrlSettings
retrieveUrlSettings = do
  storage <- localStorage
  repo <- getItem urlRepoKey storage
  lastPort <- getItem lastPortKey storage
  lastUrl <- getItem lastUrlKey storage
  pure
    $ { repo: fromMaybe (Urls.repository PublicApi) (repo >>= deserialize)
      , lastUrl: lastUrl >>= fromStored
      , lastPort: lastPort >>= fromStored
      }
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
    l <- hush $ A.decodeJson json :: Maybe { localhost :: Port }
    pure $ LocalHost l.localhost

  tryCustom :: A.Json -> Maybe Root
  tryCustom json = do
    c <- hush $ A.decodeJson json :: Maybe { custom :: String }
    pure (CustomBackend c.custom)

  fromStored :: forall a. A.DecodeJson a => String -> Maybe a
  fromStored s = do
    p <- hush $ A.jsonParser s
    hush $ A.decodeJson p

saveRepository :: UrlRepository -> Effect Unit
saveRepository repo = do
  storage <- localStorage
  let
    serialized = serialize repo.root
  setItem urlRepoKey serialized.repo storage
  case serialized.port of
    Just port -> setItem lastPortKey port storage
    _ -> pure unit
  case serialized.url of
    Just url -> setItem lastUrlKey url storage
    _ -> pure unit
  where
  public = "public"

  localhost i = { localhost: i }

  custom s = { custom: s }

  serialize :: Root -> { port :: Maybe String, repo :: String, url :: Maybe String }
  serialize PublicApi = { repo: public, port: Nothing, url: Nothing }

  serialize (LocalHost port) =
    { repo: A.stringify $ A.encodeJson $ localhost port
    , port: Just $ A.stringify $ A.encodeJson port
    , url: Nothing
    }

  serialize (CustomBackend addr) =
    { repo: A.stringify $ A.encodeJson $ custom addr
    , port: Nothing
    , url: Just $ A.stringify $ A.encodeJson addr
    }

savePerPage :: PerPage -> Effect Unit
savePerPage i = localStorage >>= setItem perPageKey (show i)

retrievePerPage :: Effect PerPage
retrievePerPage = do
  s <- localStorage
  mstr <- getItem perPageKey s
  let
    i = do
      str <- mstr
      fromString str
  pure $ perPage $ fromMaybe 20 i
