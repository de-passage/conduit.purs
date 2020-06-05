module API where

import Prelude

import Affjax as AJ
import Affjax.RequestBody as AJRB
import Affjax.ResponseFormat as AJRF
import Data.Argonaut as A
import Data.Article (Slug, Article)
import Data.Bifunctor (lmap)
import Data.Comment (Comment)
import Data.Either (Either(..))
import Data.HTTP.Method as HTTP
import Data.Maybe (Maybe(..), maybe)
import Data.Tag (Tag)
import Data.Token (Token, authorizationHeader)
import Data.User (Profile, User, Username)
import Effect.Aff (Aff)
import Data.Endpoint as E

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

getArticle :: Slug -> Aff (DecodedResponse Article)
getArticle s = getFromApi' (_.article :: ArticleResponse -> Article) (E.article s)

getArticles :: Aff (DecodedResponse (Array Article))
getArticles = getFromApi' (_.articles :: ArticlesResponse -> Array Article) E.articles

getUserArticles :: Username -> Aff (DecodedResponse (Array Article))
getUserArticles = E.userArticles >>> getFromApi' (_.articles :: ArticlesResponse -> Array Article)

getTaggedArticles :: Tag -> Aff (DecodedResponse (Array Article))
getTaggedArticles = E.taggedArticles >>> getFromApi' (_.articles :: ArticlesResponse -> Array Article)

getFavorites :: Username -> Aff (DecodedResponse (Array Article))
getFavorites = E.favorites >>> getFromApi' (_.articles :: ArticlesResponse -> Array Article)

getProfile :: Username -> Aff (DecodedResponse Profile)
getProfile u = getFromApi' (_.profile :: ProfileResponse -> Profile) (E.profile u)

getTags :: Aff (DecodedResponse (Array Tag))
getTags = getFromApi' (_.tags :: TagsResponse -> Array Tag) E.tags

getComments :: Slug -> Aff (DecodedResponse (Array Comment))
getComments slug = getFromApi' (_.comments :: CommentsResponse -> Array Comment) (E.comments slug)

login :: { email :: String, password :: String } -> Aff (DecodedResponse User)
login user = postToApi' (_.user :: UserResponse -> User) E.login { user }

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
  (resp -> ret) -> E.Endpoint -> req -> Aff (DecodedResponse ret)
postToApi' f s r = do
  resp <- postToApi (show s) $ Just $ AJRB.Json $ A.encodeJson r
  pure (f <$> resp)

getFromApi :: forall a. A.DecodeJson a => E.Endpoint -> Aff (DecodedResponse a)
getFromApi s = AJ.get AJRF.json (show s) <#> fromApiResponse

getFromApi' :: forall a b. A.DecodeJson a => (a -> b) -> E.Endpoint -> Aff (DecodedResponse b)
getFromApi' f url = do
  resp <- getFromApi url
  pure (f <$> resp)

getFeed :: Token -> Aff (DecodedResponse (Array Article))
getFeed token = do
  resp <-
    AJ.request
      $ AJ.defaultRequest
          { url = show E.feed
          , headers = [ authorizationHeader token ]
          , responseFormat = AJRF.json
          }
  let decoded = (fromApiResponse resp :: DecodedResponse ArticlesResponse)
  pure (_.articles <$> decoded)

type APIRequest =
  { token :: Maybe Token
  , method :: HTTP.Method
  , url :: E.Endpoint
  } 

apiRequest :: APIRequest -> Aff (Either AJ.Error (AJ.Response A.Json))
apiRequest { token, method, url } =
    AJ.request
      $ AJ.defaultRequest
          { url = show url
          , method = Left method
          , headers = maybe [] (\t -> [ authorizationHeader t ]) token
          , responseFormat = AJRF.json
          }
