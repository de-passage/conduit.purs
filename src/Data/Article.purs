module Data.Article
  ( Slug(..)
  , Article(..)
  , ArticleList(..)
  , class OverArticles
  , overArticles
  , class FromArticles
  , fromArticles
  , _articles
  , _articlesCount
  , ArticleIdentity(..)
  ) where

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Lens (Lens', over, traversed, view)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (SProxy(..))
import Data.Tag (Tag)
import Data.Traversable (class Traversable)
import Data.User (Profile)
import Prelude (class Eq, class Show, map, (<<<))

newtype Slug
  = Slug String

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

newtype ArticleList
  = ArticleList
  { articles :: Array Article
  , articlesCount :: Int
  }

derive instance newtypeArticleList :: Newtype ArticleList _

class OverArticles o where
  overArticles :: (Article -> Article) -> o -> o

class
  Traversable t <= FromArticles o t where
  fromArticles :: forall a. (Article -> a) -> o -> t a

_articles :: Lens' ArticleList (Array Article)
_articles = _Newtype <<< prop (SProxy :: SProxy "articles")

_articlesCount :: Lens' ArticleList Int
_articlesCount = _Newtype <<< prop (SProxy :: SProxy "articlesCount")

instance overArticlesArticleList :: OverArticles ArticleList where
  overArticles = over (_articles <<< traversed)

instance fromArticlesArticleList :: FromArticles ArticleList Array where
  fromArticles f as = map f (view _articles as)

newtype ArticleIdentity
  = ArticleIdentity Article

derive instance newtypeArticleIdentity :: Newtype ArticleIdentity _

instance overArticlesArticleIdentity :: OverArticles ArticleIdentity where
  overArticles = over _Newtype
