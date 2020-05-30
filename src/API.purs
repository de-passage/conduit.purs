module API where

import Prelude
import Affjax as AJ
import Affjax.ResponseFormat as AJRF
import Data.Argonaut as A
import Data.Article (Slug(..))
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.User (Email, Username(..))
import Effect.Aff (Aff)

newtype Token
  = Token String

derive newtype instance encodeJsonToken :: A.EncodeJson Token

derive newtype instance decodeJsonToken :: A.DecodeJson Token

type User
  = ProfileBase
      ( email :: Email
      , token :: Token
      )

type Profile
  = ProfileBase ( following :: String )

type ProfileBase r
  = { username :: Username
    , bio :: String
    , image :: String
    | r
    }

type ProfileResponse
  = { profile :: Profile
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

type ArticleResponse
  = { article :: Article
    }

type ArticlesResponse
  = { articles :: Array Article
    }

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
article (Slug s) = articles <> s

profile :: Username -> String
profile (Username u) = root <> "profiles/" <> u

follow :: Username -> String
follow u = profile u <> "/follow"

getArticle :: Slug -> Aff (Either String ArticleResponse)
getArticle s = getFromApi (article s)

getArticles :: Aff (Either String ArticlesResponse)
getArticles = getFromApi articles

getProfile :: Username -> Aff (Either String ProfileResponse)
getProfile u = getFromApi (profile u)

getFromApi :: forall a. A.DecodeJson a => String -> Aff (Either String a)
getFromApi s = do
  resp <- getJson s
  let
    result = do
      resp' <- lmap AJ.printError resp
      A.decodeJson resp'.body
  pure result

getJson :: String -> Aff (Either AJ.Error (AJ.Response A.Json))
getJson = AJ.get AJRF.json
