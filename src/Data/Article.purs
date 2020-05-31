module Data.Article where 

import Data.Newtype

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.User (Profile)

newtype Slug = Slug String

derive instance newtypeSlug :: Newtype Slug _
derive newtype instance decodeJsonSlug :: DecodeJson Slug
derive newtype instance encodeJsonSlug :: EncodeJson Slug

type Article
  = { slug :: Slug
    , title :: String
    , description :: String
    , body :: String
    , tagList :: Array String
    , createdAt :: String
    , updatedAt :: String
    , favorited :: Boolean
    , favoritesCount :: Int
    , author :: Profile
    }