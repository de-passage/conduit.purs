module Data.Comment (Comment, CommentId) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Newtype (class Newtype)
import Data.User (Profile)

newtype CommentId = CommentId Int

derive instance newtypeCommentId :: Newtype CommentId _
derive newtype instance decodeJsonCommentId :: DecodeJson CommentId
derive newtype instance encodeJsonCommentId :: EncodeJson CommentId

instance showCommentId :: Show CommentId where
  show (CommentId id) = show id

type Comment
  = { id :: CommentId
    , createdAt :: String
    , updatedAt :: String
    , body :: String
    , author :: Profile
    }
