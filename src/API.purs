module API where

import Prelude

import Affjax as AJ
import Affjax.RequestBody as AJRB
import Affjax.ResponseFormat as AJRF
import Data.Argonaut as A
import Data.Article (Slug(..), Article)
import Data.Bifunctor (lmap)
import Data.Comment (Comment)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Tag (Tag(..))
import Data.User (Profile, Username(..), User)
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

type CommentsResponse
  = { comments :: Array Comment }

type TagsResponse
  = { tags :: Array Tag }

root :: String
root = "https://conduit.productionready.io/api/"

articles :: String
articles = root <> "articles/"

article :: Slug -> String
article (Slug s) = articles <> s <> "/"

profile :: Username -> String
profile (Username u) = root <> "profiles/" <> u <> "/"

follow :: Username -> String
follow u = profile u <> "follow/"

tags :: String
tags = root <> "tags/"

comments :: Slug -> String
comments slug = article slug <> "comments/"

users :: String
users = root <> "users/"

loginUrl :: String
loginUrl = users <> "login/"

userArticles :: Username -> String
userArticles (Username u) = articles <> "?author=" <> u

taggedArticles :: Tag -> String
taggedArticles (Tag t) = articles <> "?tag=" <> t

favorites :: Username -> String
favorites (Username u) = articles <> "?favorited=" <> u

getArticle :: Slug -> Aff (Either String Article)
getArticle s = getFromApi' (_.article :: ArticleResponse -> Article) (article s)

getArticles :: Aff (Either String (Array Article))
getArticles = getFromApi' (_.articles :: ArticlesResponse -> Array Article) articles

getUserArticles :: Username -> Aff (Either String (Array Article))
getUserArticles = userArticles >>> getFromApi' (_.articles :: ArticlesResponse -> Array Article)

getTaggedArticles :: Tag -> Aff (Either String (Array Article))
getTaggedArticles = taggedArticles >>> getFromApi' (_.articles :: ArticlesResponse -> Array Article)

getFavorites :: Username -> Aff (Either String (Array Article))
getFavorites = favorites >>> getFromApi' (_.articles :: ArticlesResponse -> Array Article)

getProfile :: Username -> Aff (Either String Profile)
getProfile u = getFromApi' (_.profile :: ProfileResponse -> Profile) (profile u)

getTags :: Aff (Either String (Array Tag))
getTags = getFromApi' (_.tags :: TagsResponse -> Array Tag) tags

getComments :: Slug -> Aff (Either String (Array Comment))
getComments slug = getFromApi' (_.comments :: CommentsResponse -> Array Comment) (comments slug)

postToApi :: forall a. A.DecodeJson a => String -> Maybe AJRB.RequestBody -> Aff (Either String a)
postToApi s r = AJ.post AJRF.json s r <#> fromApiResponse

login :: forall a. A.DecodeJson a => { email::String, password::String } -> Aff (Either String User)
login = postToApi loginUrl <<< Just <<< AJRB.Json <<< A.encodeJson

fromApiResponse :: forall a. A.DecodeJson a => Either AJ.Error (AJ.Response A.Json) -> Either String a
fromApiResponse a = do
  resp <- lmap AJ.printError a
  A.decodeJson resp.body

getFromApi :: forall a. A.DecodeJson a => String -> Aff (Either String a)
getFromApi s = getJson s <#> fromApiResponse

getFromApi' :: forall a b. A.DecodeJson a => (a -> b) -> String -> Aff (Either String b)
getFromApi' f url = do
  resp <- getFromApi url
  pure (f <$> resp)

getJson :: String -> Aff (Either AJ.Error (AJ.Response A.Json))
getJson = AJ.get AJRF.json
