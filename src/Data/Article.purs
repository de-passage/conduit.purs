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
  , PerPage
  , Offset
  , PageNumber
  , ArticleDisplaySettings
  , pageNumber
  , mkDisplaySettings
  , perPage
  , offset
  , firstPage
  ) where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Lens (Lens', over, traversed, view)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (SProxy(..))
import Data.Tag (Tag)
import Data.Traversable (class Traversable)
import Data.User (Profile)

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

newtype PerPage
  = PerPage Int

instance showPerPage :: Show PerPage where
  show (PerPage i) = show i

derive instance eqPerPage :: Eq PerPage

newtype Offset
  = Offset Int

newtype PageNumber
  = PageNumber Int

instance showOffset :: Show Offset where
  show (Offset i) = show i

newtype ArticleDisplaySettings
  = ArticleDisplaySettings
  { offset :: Offset
  , perPage :: PerPage
  }

pageNumber :: Int -> PageNumber
pageNumber = PageNumber <<< max 0

perPage :: Int -> PerPage
perPage = PerPage <<< max 1

offset :: Int -> Offset
offset = Offset <<< max 0

firstPage :: PerPage -> ArticleDisplaySettings
firstPage = mkDisplaySettings (PageNumber 1)

mkDisplaySettings :: PageNumber -> PerPage -> ArticleDisplaySettings
mkDisplaySettings (PageNumber pn) pPage@(PerPage pp) =
  ArticleDisplaySettings
    { offset: Offset $ (pn - 1) * pp
    , perPage: pPage
    }
