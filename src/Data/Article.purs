module Data.Article (Slug(..), Article(..)) where 

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Newtype (class Newtype, unwrap)
import Data.Tag (Tag)
import Data.User (Profile)
import Prelude (class Eq, class Show)

newtype Slug = Slug String

derive instance newtypeSlug :: Newtype Slug _
derive newtype instance decodeJsonSlug :: DecodeJson Slug
derive newtype instance encodeJsonSlug :: EncodeJson Slug
derive instance eqSlug :: Eq Slug

instance showSlug :: Show Slug where
  show = unwrap

type Article
  = { slug :: Slug
    , title :: String
    , description :: String
    , body :: String
    , tagList :: Array Tag
    , createdAt :: String
    , updatedAt :: String
    , favorited :: Boolean
    , favoritesCount :: Int
    , author :: Profile
    }