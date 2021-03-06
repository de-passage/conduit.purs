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
import Data.Article (Article, ArticleList(..), Offset, PerPage, Slug)
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
          (AJ.StatusCode 422) -> Left (ValidationFailed $ Utils.parseAPIErrors body)
          (AJ.StatusCode 401) -> Left Unauthorized
          (AJ.StatusCode 403) -> Left Forbidden
          (AJ.StatusCode 200) -> case A.decodeJson body of
            Left err -> Left $ ParseError err
            Right ok -> Right ok
          (AJ.StatusCode 500) -> Left (InternalServerError $ Utils.parseAPIErrors body)
          (AJ.StatusCode unknown) -> Left (APIError unknown)

getArticle :: UrlRepository -> Slug -> Maybe Token -> Aff (Response Article)
getArticle url s t = (request $ E.article url s t) <%> _.article

getArticles :: UrlRepository -> Offset -> PerPage -> Maybe Token -> Aff (Response ArticleList)
getArticles url offset perPage token =
  ( request
      $ E.articles url
          (Url.defaultArticleOptions { offset = Just offset, limit = Just perPage })
          token
  )
    <%> ArticleList

getUserArticles :: UrlRepository -> Username -> Offset -> PerPage -> Maybe Token -> Aff (Response ArticleList)
getUserArticles url user offset perPage token =
  ( request
      $ E.articles url
          ( Url.defaultArticleOptions
              { author = Just user
              , offset = Just offset
              , limit = Just perPage
              }
          )
          token
  )
    <%> ArticleList

getTaggedArticles :: UrlRepository -> Tag -> Offset -> PerPage -> Maybe Token -> Aff (Response ArticleList)
getTaggedArticles url tag offset perPage token =
  ( request
      $ E.articles url
          ( Url.defaultArticleOptions
              { tag = Just tag
              , offset = Just offset
              , limit = Just perPage
              }
          )
          token
  )
    <%> ArticleList

getFavorites :: UrlRepository -> Username -> Offset -> PerPage -> Maybe Token -> Aff (Response ArticleList)
getFavorites url user offset perPage token =
  ( request
      $ E.articles url
          ( Url.defaultArticleOptions
              { favorited = Just user
              , offset = Just offset
              , limit = Just perPage
              }
          )
          token
  )
    <%> ArticleList

getProfile :: UrlRepository -> Username -> Maybe Token -> Aff (Response Profile)
getProfile url u token = (request $ E.profile url u token) <%> _.profile

getTags :: UrlRepository -> Aff (Response (Array Tag))
getTags url = (request $ E.tags url) <%> _.tags

getComments :: UrlRepository -> Slug -> Maybe Token -> Aff (Response (Array Comment))
getComments url slug token = (request $ E.comments url slug token) <%> _.comments

getFeed :: UrlRepository -> PerPage -> Offset -> Token -> Aff (Response ArticleList)
getFeed url perPage offset token =
  ( request
      $ E.limitedFeed url
          { limit: Just perPage
          , offset: Just offset
          }
          token
  )
    <%> ArticleList

loginR :: UrlRepository -> { email :: String, password :: String } -> Aff (Response User)
loginR url user = (request $ E.login url { user }) <%> _.user

nmap :: forall f g a b. Functor f => Functor g => f (g a) -> (a -> b) -> f (g b)
nmap = flip (map <<< map)

infixl 2 nmap as <%>
