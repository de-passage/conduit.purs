module API (module E
  , getArticle
  , getArticles
  , getComments
  , getUserArticles
  , getTaggedArticles
  , getTags
  , getFavorites
  , getFeed
  , loginR
  , getProfile
  , request) where

import API.Response
import Prelude

import API.Endpoint as E
import API.Endpoint.Core (Request)
import API.Url as Url
import Affjax (request, printError) as AJ
import Affjax.StatusCode (StatusCode(..)) as AJ
import Data.Argonaut as A
import Data.Article (Slug, Article)
import Data.Bifunctor (bimap)
import Data.Comment (Comment)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tag (Tag)
import Data.Token (Token)
import Data.User (Profile, User, Username)
import Effect.Aff (Aff)

request :: forall a. A.DecodeJson a => Request a -> Aff (Response a)
request r = do
  req <- AJ.request $ unwrap r
  pure $ 
    case req of
      Left err -> Left $ AjaxFailed $ AJ.printError err
      Right { body, status } -> 
        case status of
          (AJ.StatusCode 404) -> Left NotFound
          (AJ.StatusCode 422) -> Left (ValidationFailed [])
          (AJ.StatusCode 401) -> Left Unauthorized
          (AJ.StatusCode 403) -> Left Forbidden
          (AJ.StatusCode 200) ->
            case A.decodeJson body of
              Left err -> Left $ ParseError err
              Right ok -> Right ok
          _ -> Left APIError

type DecodedResponse a
  = Either String a

hack :: Error -> String
hack _ = "Oops"

request' :: forall r d. A.DecodeJson r => (r -> d) -> Request r -> Aff (DecodedResponse d)
request' f r = request r >>= bimap hack f >>> pure

getArticle :: Slug -> Maybe Token -> Aff (DecodedResponse Article)
getArticle s t = request' (_.article :: ArticleResponse -> Article) $ E.article s t

getArticles :: Maybe Token -> Aff (DecodedResponse (Array Article))
getArticles = request' (_.articles :: ArticlesResponse -> Array Article) <<< E.allArticles

getUserArticles :: Username -> Maybe Token -> Aff (DecodedResponse (Array Article))
getUserArticles user token =
  request' (_.articles :: ArticlesResponse -> Array Article)
    $ E.articles (Url.defaultArticleOptions { author = Just user }) token

getTaggedArticles :: Tag -> Maybe Token -> Aff (DecodedResponse (Array Article))
getTaggedArticles tag token =
  request' (_.articles :: ArticlesResponse -> Array Article)
    $ E.articles (Url.defaultArticleOptions { tag = Just tag }) token

getFavorites :: Username -> Maybe Token -> Aff (DecodedResponse (Array Article))
getFavorites user token = request' (_.articles :: ArticlesResponse -> Array Article)
    $ E.articles (Url.defaultArticleOptions { favorited = Just user }) token

getProfile :: Username -> Maybe Token -> Aff (DecodedResponse Profile)
getProfile u token = request' (_.profile :: ProfileResponse -> Profile) $ E.profile u token

getTags :: Aff (DecodedResponse (Array Tag))
getTags = request' (_.tags :: TagsResponse -> Array Tag) E.tags

getComments :: Slug -> Maybe Token -> Aff (DecodedResponse (Array Comment))
getComments slug token = request' (_.comments :: CommentsResponse -> Array Comment) (E.comments slug token)

getFeed :: Token -> Aff (DecodedResponse (Array Article))
getFeed token = request' (_.articles :: ArticlesResponse -> Array Article) (E.feed token)

loginR :: { email :: String, password :: String } -> Aff (DecodedResponse User)
loginR user = request' (_.user :: UserResponse -> User) $ E.login { user }
