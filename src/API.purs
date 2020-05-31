module API where

import Prelude
import Affjax as AJ
import Affjax.ResponseFormat as AJRF
import Data.Argonaut as A
import Data.Article (Slug(..), Article)
import Data.Bifunctor (lmap)
import Data.Comment (Comment)
import Data.Either (Either)
import Data.User (Username(..), Profile)
import Effect.Aff (Aff)

type ProfileResponse
  = { profile :: Profile
    }

type ArticleResponse
  = { article :: Article
    }

type ArticlesResponse
  = { articles :: Array Article
    }

type CommentResponse
  = { comment :: Comment
    }

type Comments
  = { comments :: Array Comment }

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
