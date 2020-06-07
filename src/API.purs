module API
  ( module E
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
  , request
  ) where

import Prelude
import API.Response
  ( ArticleResponse
  , ArticlesResponse
  , CommentsResponse
  , Error(..)
  , ProfileResponse
  , Response
  , TagsResponse
  , UserResponse
  )
import API.Endpoint
  ( ArticleCreationPayload
  , ArticleEditionPayload
  , ArticleRequest
  , ArticlesRequest
  , CommentPayload
  , CommentRequest
  , CommentsRequest
  , LoginPayload
  , ProfileRequest
  , RegistrationPayload
  , SimpleRequest
  , TagsRequest
  , UserRequest
  , UserUpdatePayload
  , allArticles
  , article
  , articleCreation
  , articleDeletion
  , articleEdition
  , articles
  , commentCreation
  , commentDeletion
  , comments
  , currentUser
  , favorite
  , feed
  , follow
  , limitedFeed
  , login
  , profile
  , registation
  , tags
  , unfavorite
  , unfollow
  , updateUser
  )
  as E
import API.Endpoint.Core (Request)
import API.Url as Url
import Affjax (request, printError) as AJ
import Affjax.StatusCode (StatusCode(..)) as AJ
import Data.Argonaut as A
import Data.Article (Slug, Article)
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
  pure
    $ case req of
        Left err -> Left $ AjaxFailed $ AJ.printError err
        Right { body, status } -> case status of
          (AJ.StatusCode 404) -> Left NotFound
          (AJ.StatusCode 422) -> Left (ValidationFailed [])
          (AJ.StatusCode 401) -> Left Unauthorized
          (AJ.StatusCode 403) -> Left Forbidden
          (AJ.StatusCode 200) -> case A.decodeJson body of
            Left err -> Left $ ParseError err
            Right ok -> Right ok
          (AJ.StatusCode unknown) -> Left (APIError unknown)

getArticle :: Slug -> Maybe Token -> Aff (Response Article)
getArticle s t = (request $ E.article s t) <%> _.article

getArticles :: Maybe Token -> Aff (Response (Array Article))
getArticles token = (request $ E.allArticles token) <%> _.articles

getUserArticles :: Username -> Maybe Token -> Aff (Response (Array Article))
getUserArticles user token =
  ( request
        $ E.articles (Url.defaultArticleOptions { author = Just user }) token
    )
     <%> _.articles

getTaggedArticles :: Tag -> Maybe Token -> Aff (Response (Array Article))
getTaggedArticles tag token =
  (request
    $ E.articles (Url.defaultArticleOptions { tag = Just tag }) token) <%> _.articles

getFavorites :: Username -> Maybe Token -> Aff (Response (Array Article))
getFavorites user token = (request $ E.articles (Url.defaultArticleOptions { favorited = Just user }) token) <%> _.articles

getProfile :: Username -> Maybe Token -> Aff (Response Profile)
getProfile u token = (request $ E.profile u token) <%> _.profile

getTags :: Aff (Response (Array Tag))
getTags = (request E.tags) <%> _.tags

getComments :: Slug -> Maybe Token -> Aff (Response (Array Comment))
getComments slug token = (request $ E.comments slug token) <%> _.comments

getFeed :: Token -> Aff (Response (Array Article))
getFeed token = (request $ E.feed token) <%> _.articles

loginR :: { email :: String, password :: String } -> Aff (Response User)
loginR user = (request $ E.login { user }) <%> _.user

nmap :: forall f g a b. Functor f => Functor g => f (g a) -> (a -> b) -> f (g b)
nmap = flip (map <<< map)
infixl 2 nmap as <%> 