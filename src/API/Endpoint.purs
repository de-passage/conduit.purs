module API.Endpoint where

--import Prelude

import API.Endpoint.Core as C
import API.Endpoint.Descriptions as D
import API.Response as R
import API.Url (ArticleOptions, ArticleLimit)
import API.Url as Url
import Data.Article (Slug)
import Data.Comment (CommentId)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe)
import Data.Token as T
import Data.User (Username)
import Prelude (Unit)

type LoginPayload = Record D.LoginPayload
type RegistrationPayload = Record D.RegistrationPayload
type UserUpdatePayload = Record D.UserUpdatePayload
type ArticleCreationPayload = Record D.ArticleCreationPayload
type ArticleEditionPayload = Record D.ArticleEditionPayload
type CommentPayload = Record D.CommentPayload

type UserRequest = C.Request R.UserResponse
type ProfileRequest = C.Request R.ProfileResponse 
type TagsRequest = C.Request R.TagsResponse
type ArticleRequest = C.Request R.ArticleResponse
type ArticlesRequest = C.Request R.ArticlesResponse
type CommentRequest = C.Request R.CommentResponse
type CommentsRequest = C.Request R.CommentsResponse
type SimpleRequest = C.Request Unit

login :: LoginPayload -> UserRequest
login = C.create D.login Url.login POST

registration :: RegistrationPayload -> UserRequest
registration = C.create D.register Url.register POST

currentUser :: T.Token -> UserRequest
currentUser = C.create_ D.currentUser Url.currentUser GET

updateUser :: UserUpdatePayload -> T.Token -> UserRequest
updateUser = C.create D.updateUser Url.currentUser PUT

profile :: Username -> Maybe T.Token -> ProfileRequest
profile user = C.create_ D.profile (Url.profile user) GET

follow :: Username -> T.Token -> ProfileRequest
follow user = C.create_ D.follow (Url.follow user) POST

unfollow :: Username -> T.Token -> ProfileRequest
unfollow user = C.create_ D.follow (Url.follow user) DELETE

articles :: ArticleOptions -> Maybe T.Token -> ArticlesRequest
articles options = C.create_ D.articles (Url.articles options) GET

allArticles :: Maybe T.Token -> ArticlesRequest
allArticles = C.create_ D.articles Url.allArticles GET

limitedFeed :: ArticleLimit -> T.Token -> ArticlesRequest
limitedFeed options = C.create_ D.feed (Url.limitedFeed options) GET

feed :: T.Token -> ArticlesRequest
feed = C.create_ D.feed Url.feed GET

article :: Slug -> Maybe T.Token -> ArticleRequest
article s = C.create_ D.article (Url.article s) GET

articleCreation :: ArticleCreationPayload -> T.Token -> ArticleRequest
articleCreation = C.create D.createArticle Url.allArticles POST

articleEdition :: Slug -> ArticleEditionPayload -> T.Token -> ArticleRequest
articleEdition slug = C.create D.editArticle (Url.article slug) PUT

articleDeletion :: Slug -> T.Token -> SimpleRequest
articleDeletion slug = C.create_ D.deleteArticle (Url.article slug) DELETE

commentCreation :: Slug -> CommentPayload -> T.Token -> CommentRequest
commentCreation slug = C.create D.comment (Url.comments slug) POST

comments :: Slug -> Maybe T.Token -> CommentsRequest
comments slug = C.create_ D.comments (Url.comments slug) GET

commentDeletion :: Slug -> CommentId -> T.Token -> SimpleRequest
commentDeletion slug id = C.create_ D.deleteComment (Url.comment slug id) DELETE

favorite :: Slug -> T.Token -> ArticleRequest
favorite slug = C.create_ D.favorite (Url.favorite slug) POST

unfavorite :: Slug -> T.Token -> ArticleRequest
unfavorite slug = C.create_ D.favorite (Url.favorite slug) DELETE

tags :: TagsRequest
tags = C.create_ D.tags Url.tags GET