module Data.Article where 

import Data.Newtype

import Data.Argonaut (class DecodeJson, class EncodeJson)

newtype Slug = Slug String

derive instance newtypeSlug :: Newtype Slug _
derive newtype instance decodeJsonSlug :: DecodeJson Slug
derive newtype instance encodeJsonSlug :: EncodeJson Slug