module API where

import Prelude

import Affjax as AJ
import Affjax.RequestBody as AJRB
import Affjax.RequestHeader as AJRH
import Affjax.ResponseFormat as AJRF
import Data.Argonaut as A
import Data.Article (Slug(..), Article)
import Data.Bifunctor (lmap)
import Data.Comment (Comment)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tag (Tag(..))
import Data.User (Profile, Token, User, Username(..), authorizationHeader)
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

type UserResponse
  = { user :: User }

type DecodedResponse a
  = Either String a

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

feed :: String
feed = articles <> "feed/"

loginUrl :: String
loginUrl = users <> "login/"

userArticles :: Username -> String
userArticles (Username u) = articles <> "?author=" <> u

taggedArticles :: Tag -> String
taggedArticles (Tag t) = articles <> "?tag=" <> t

favorites :: Username -> String
favorites (Username u) = articles <> "?favorited=" <> u

getArticle :: Slug -> Aff (DecodedResponse Article)
getArticle s = getFromApi' (_.article :: ArticleResponse -> Article) (article s)

getArticles :: Aff (DecodedResponse (Array Article))
getArticles = getFromApi' (_.articles :: ArticlesResponse -> Array Article) articles

getUserArticles :: Username -> Aff (DecodedResponse (Array Article))
getUserArticles = userArticles >>> getFromApi' (_.articles :: ArticlesResponse -> Array Article)

getTaggedArticles :: Tag -> Aff (DecodedResponse (Array Article))
getTaggedArticles = taggedArticles >>> getFromApi' (_.articles :: ArticlesResponse -> Array Article)

getFavorites :: Username -> Aff (DecodedResponse (Array Article))
getFavorites = favorites >>> getFromApi' (_.articles :: ArticlesResponse -> Array Article)

getProfile :: Username -> Aff (DecodedResponse Profile)
getProfile u = getFromApi' (_.profile :: ProfileResponse -> Profile) (profile u)

getTags :: Aff (DecodedResponse (Array Tag))
getTags = getFromApi' (_.tags :: TagsResponse -> Array Tag) tags

getComments :: Slug -> Aff (DecodedResponse (Array Comment))
getComments slug = getFromApi' (_.comments :: CommentsResponse -> Array Comment) (comments slug)

login :: { email :: String, password :: String } -> Aff (DecodedResponse User)
login user = postToApi' (_.user :: UserResponse -> User) loginUrl { user }

fromApiResponse :: forall a. A.DecodeJson a => Either AJ.Error (AJ.Response A.Json) -> DecodedResponse a
fromApiResponse a = do
  resp <- lmap AJ.printError a
  A.decodeJson resp.body

postToApi :: forall a. A.DecodeJson a => String -> Maybe AJRB.RequestBody -> Aff (DecodedResponse a)
postToApi s r = AJ.post AJRF.json s r <#> fromApiResponse

postToApi' ::
  forall resp ret req.
  A.DecodeJson resp =>
  A.EncodeJson req =>
  (resp -> ret) -> String -> req -> Aff (DecodedResponse ret)
postToApi' f s r = do
  resp <- postToApi s $ Just $ AJRB.Json $ A.encodeJson r
  pure (f <$> resp)

getFromApi :: forall a. A.DecodeJson a => String -> Aff (DecodedResponse a)
getFromApi s = AJ.get AJRF.json s <#> fromApiResponse

getFromApi' :: forall a b. A.DecodeJson a => (a -> b) -> String -> Aff (DecodedResponse b)
getFromApi' f url = do
  resp <- getFromApi url
  pure (f <$> resp)

getFeed :: Token -> Aff (DecodedResponse (Array Article))
getFeed token = do
  resp <-
    AJ.request
      $ AJ.defaultRequest
          { url = feed
          , headers = [ authorizationHeader token ]
          , responseFormat = AJRF.json
          }
  let decoded = (fromApiResponse resp :: DecodedResponse (Array Article))
  pure decoded
