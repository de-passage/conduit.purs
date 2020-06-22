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
import API.Endpoint (ArticleCreationPayload, ArticleEditionPayload, ArticleRequest, ArticlesRequest, CommentPayload, CommentRequest, CommentsRequest, LoginPayload, ProfileRequest, RegistrationPayload, SimpleRequest, TagsRequest, UserRequest, UserUpdatePayload, allArticles, article, articleCreation, articleDeletion, articleEdition, articles, commentCreation, commentDeletion, comments, currentUser, favorite, feed, follow, limitedFeed, login, profile, registration, tags, unfavorite, unfollow, updateUser) as E
import API.Endpoint.Core (Request)
import API.Response (Error(..), Response)
import API.Url (UrlRepository)
import API.Url as Url
import API.Utils as Utils
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
          (AJ.StatusCode 422) -> Left (ValidationFailed $ Utils.parseValidationErrors body)
          (AJ.StatusCode 401) -> Left Unauthorized
          (AJ.StatusCode 403) -> Left Forbidden
          (AJ.StatusCode 200) -> case A.decodeJson body of
            Left err -> Left $ ParseError err
            Right ok -> Right ok
          (AJ.StatusCode unknown) -> Left (APIError unknown)

getArticle :: UrlRepository -> Slug -> Maybe Token -> Aff (Response Article)
getArticle url s t = (request $ E.article url s t) <%> _.article

getArticles :: UrlRepository -> Maybe Token -> Aff (Response (Array Article))
getArticles url token = (request $ E.allArticles url token) <%> _.articles

getUserArticles :: UrlRepository -> Username -> Maybe Token -> Aff (Response (Array Article))
getUserArticles url user token =
  ( request
      $ E.articles url (Url.defaultArticleOptions { author = Just user }) token
  )
    <%> _.articles

getTaggedArticles :: UrlRepository -> Tag -> Maybe Token -> Aff (Response (Array Article))
getTaggedArticles url tag token =
  ( request
      $ E.articles url (Url.defaultArticleOptions { tag = Just tag }) token
  )
    <%> _.articles

getFavorites :: UrlRepository -> Username -> Maybe Token -> Aff (Response (Array Article))
getFavorites url user token = (request $ E.articles url (Url.defaultArticleOptions { favorited = Just user }) token) <%> _.articles

getProfile :: UrlRepository -> Username -> Maybe Token -> Aff (Response Profile)
getProfile url u token = (request $ E.profile url u token) <%> _.profile

getTags :: UrlRepository -> Aff (Response (Array Tag))
getTags url = (request $ E.tags url) <%> _.tags

getComments :: UrlRepository -> Slug -> Maybe Token -> Aff (Response (Array Comment))
getComments url slug token = (request $ E.comments url slug token) <%> _.comments

getFeed :: UrlRepository -> Token -> Aff (Response (Array Article))
getFeed url token = (request $ E.feed url token) <%> _.articles

loginR :: UrlRepository -> { email :: String, password :: String } -> Aff (Response User)
loginR url user = (request $ E.login url { user }) <%> _.user

nmap :: forall f g a b. Functor f => Functor g => f (g a) -> (a -> b) -> f (g b)
nmap = flip (map <<< map)

infixl 2 nmap as <%>
