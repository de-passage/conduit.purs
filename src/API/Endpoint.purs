module API.Endpoint where

--import Prelude
import API.Endpoint.Core as C
import API.Endpoint.Descriptions as D
import API.Response as R
import API.Url (ArticleLimit, ArticleOptions, UrlRepository)
import Data.Article (Slug)
import Data.Comment (CommentId)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe)
import Data.Token as T
import Data.User (Username)
import Prelude (Unit)

type LoginPayload
  = Record D.LoginPayload

type RegistrationPayload
  = Record D.RegistrationPayload

type UserUpdatePayload
  = Record D.UserUpdatePayload

type ArticleCreationPayload
  = Record D.ArticleCreationPayload

type ArticleEditionPayload
  = Record D.ArticleEditionPayload

type CommentPayload
  = Record D.CommentPayload

type UserRequest
  = C.Request R.UserResponse

type ProfileRequest
  = C.Request R.ProfileResponse

type TagsRequest
  = C.Request R.TagsResponse

type ArticleRequest
  = C.Request R.ArticleResponse

type ArticlesRequest
  = C.Request R.ArticlesResponse

type CommentRequest
  = C.Request R.CommentResponse

type CommentsRequest
  = C.Request R.CommentsResponse

type SimpleRequest
  = C.Request Unit

login :: UrlRepository -> LoginPayload -> UserRequest
login url = C.create D.login url.login POST

registration :: UrlRepository -> RegistrationPayload -> UserRequest
registration url = C.create D.register url.register POST

currentUser :: UrlRepository -> T.Token -> UserRequest
currentUser url = C.create_ D.currentUser url.currentUser GET

updateUser :: UrlRepository -> UserUpdatePayload -> T.Token -> UserRequest
updateUser url = C.create D.updateUser url.currentUser PUT

profile :: UrlRepository -> Username -> Maybe T.Token -> ProfileRequest
profile url user = C.create_ D.profile (url.profile user) GET

follow :: UrlRepository -> Username -> T.Token -> ProfileRequest
follow url user = C.create_ D.follow (url.follow user) POST

unfollow :: UrlRepository -> Username -> T.Token -> ProfileRequest
unfollow url user = C.create_ D.follow (url.follow user) DELETE

articles :: UrlRepository -> ArticleOptions -> Maybe T.Token -> ArticlesRequest
articles url options = C.create_ D.articles (url.articles options) GET

allArticles :: UrlRepository -> Maybe T.Token -> ArticlesRequest
allArticles url = C.create_ D.articles url.allArticles GET

limitedFeed :: UrlRepository -> ArticleLimit -> T.Token -> ArticlesRequest
limitedFeed url options = C.create_ D.feed (url.limitedFeed options) GET

feed :: UrlRepository -> T.Token -> ArticlesRequest
feed url = C.create_ D.feed url.feed GET

article :: UrlRepository -> Slug -> Maybe T.Token -> ArticleRequest
article url s = C.create_ D.article (url.article s) GET

articleCreation :: UrlRepository -> ArticleCreationPayload -> T.Token -> ArticleRequest
articleCreation url = C.create D.createArticle url.allArticles POST

articleEdition :: UrlRepository -> Slug -> ArticleEditionPayload -> T.Token -> ArticleRequest
articleEdition url slug = C.create D.editArticle (url.article slug) PUT

articleDeletion :: UrlRepository -> Slug -> T.Token -> SimpleRequest
articleDeletion url slug = C.create_ D.deleteArticle (url.article slug) DELETE

commentCreation :: UrlRepository -> Slug -> CommentPayload -> T.Token -> CommentRequest
commentCreation url slug = C.create D.comment (url.comments slug) POST

comments :: UrlRepository -> Slug -> Maybe T.Token -> CommentsRequest
comments url slug = C.create_ D.comments (url.comments slug) GET

commentDeletion :: UrlRepository -> Slug -> CommentId -> T.Token -> SimpleRequest
commentDeletion url slug id = C.create_ D.deleteComment (url.comment slug id) DELETE

favorite :: UrlRepository -> Slug -> T.Token -> ArticleRequest
favorite url slug = C.create_ D.favorite (url.favorite slug) POST

unfavorite :: UrlRepository -> Slug -> T.Token -> ArticleRequest
unfavorite url slug = C.create_ D.favorite (url.favorite slug) DELETE

tags :: UrlRepository -> TagsRequest
tags url = C.create_ D.tags url.tags GET
