module API where

import Prelude

import Affjax as AJ
import Affjax.ResponseFormat as AJRF
import Data.Argonaut as A
import Data.Article (Slug(..))
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.User (Email, Username)
import Effect.Aff (Aff)

newtype Token
  = Token String

derive newtype instance encodeJsonToken :: A.EncodeJson Token

derive newtype instance decodeJsonToken :: A.DecodeJson Token

type User
  = UserBase
      ( email :: Email
      , token :: Token
      )

type Profile
  = UserBase ( following :: String )

type UserBase r
  = { username :: Username
    , bio :: String
    , image :: String
    | r
    }

type Article
  = { slug :: Slug
    , title :: String
    , description :: String
    , body :: String
    , tagList :: Array String
    , createdAt :: String
    , updatedAt :: String
    , favorited :: Boolean
    , favoritesCount :: Int
    , author :: Profile
    }

type Articles
  = Array Article

type Comment
  = { id :: Int
    , createdAt :: String
    , updatedAt :: String
    , body :: String
    , author :: Profile
    }

type Comments
  = Array Comment

root :: String
root = "https://conduit.productionready.io/api/"

articles :: String
articles = root <> "articles/"

article :: Slug -> String
article (Slug s) = articles <> s <> "/"

getArticle :: Slug -> Aff (Either String Article)
getArticle s = do
  resp <- getJson (article s)
  let
    result = do
      resp' <- lmap AJ.printError resp
      A.decodeJson resp'.body
  pure result

getJson :: String -> Aff (Either AJ.Error (AJ.Response A.Json))
getJson = AJ.get AJRF.json
