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
  , nextPage
  , previousPage
  , lastPage
  , ArticleCount
  , isFirst
  , isLast
  , toOffset
  , emptyPageNumber
  , _pageNumber
  , noOffset
  , foldPages
  , Page(..)
  , Distance(..)
  , fromPageNumber
  , pageCount
  ) where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Lens (Forget, Lens', over, to, traversed, view)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype, unwrap)
import Data.Ord (abs)
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

newtype ArticleCount
  = ArticleCount Int

derive newtype instance decodeJsonArticleCount :: DecodeJson ArticleCount

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
  , articlesCount :: ArticleCount
  }

derive instance newtypeArticleList :: Newtype ArticleList _

class OverArticles o where
  overArticles :: (Article -> Article) -> o -> o

class
  Traversable t <= FromArticles o t where
  fromArticles :: forall a. (Article -> a) -> o -> t a

_articles :: Lens' ArticleList (Array Article)
_articles = _Newtype <<< prop (SProxy :: SProxy "articles")

_articlesCount :: Lens' ArticleList ArticleCount
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

instance showPageNumber :: Show PageNumber where
  show (PageNumber i) = show i

instance showOffset :: Show Offset where
  show (Offset i) = show i

derive newtype instance eqPageNumber :: Eq PageNumber

newtype ArticleDisplaySettings
  = ArticleDisplaySettings
  { pageNumber :: PageNumber
  , perPage :: PerPage
  , articleCount :: ArticleCount
  }

pageNumber :: ArticleCount -> PerPage -> Int -> PageNumber
pageNumber (ArticleCount ac) (PerPage pp) = PageNumber <<< max 0 <<< min ((ac - 1 / pp) + 1)

perPage :: Int -> PerPage
perPage = PerPage <<< max 1

offset :: PageNumber -> PerPage -> Offset
offset (PageNumber pn) (PerPage pp) = Offset (pn * pp)

fromPageNumber :: PageNumber -> Int
fromPageNumber (PageNumber pn) = pn

firstPage :: ArticleDisplaySettings -> ArticleDisplaySettings
firstPage (ArticleDisplaySettings settings) = ArticleDisplaySettings settings { pageNumber = (PageNumber 0) }

lastPage :: ArticleDisplaySettings -> ArticleDisplaySettings
lastPage (ArticleDisplaySettings settings) = ArticleDisplaySettings settings { pageNumber = pn }
  where
  pn = computeLast settings.articleCount settings.perPage

nextPage :: ArticleDisplaySettings -> ArticleDisplaySettings
nextPage (ArticleDisplaySettings settings) =
  ArticleDisplaySettings
    settings
      { pageNumber = pageNumber settings.articleCount settings.perPage (fromPageNumber settings.pageNumber + 1) }

previousPage :: ArticleDisplaySettings -> ArticleDisplaySettings
previousPage (ArticleDisplaySettings settings) =
  ArticleDisplaySettings
    settings
      { pageNumber = pageNumber settings.articleCount settings.perPage (fromPageNumber settings.pageNumber - 1) }

computeLast :: ArticleCount -> PerPage -> PageNumber
computeLast (ArticleCount ac) (PerPage pp) = PageNumber $ max 0 $ (ac - 1) / pp

isLast :: ArticleDisplaySettings -> Boolean
isLast (ArticleDisplaySettings settings) = computeLast settings.articleCount settings.perPage == settings.pageNumber

isFirst :: ArticleDisplaySettings -> Boolean
isFirst (ArticleDisplaySettings settings) = settings.pageNumber == (PageNumber 0)

mkDisplaySettings ::
  PageNumber -> PerPage -> ArticleCount -> ArticleDisplaySettings
mkDisplaySettings pn pPage count =
  ArticleDisplaySettings
    { pageNumber: pn
    , perPage: pPage
    , articleCount: count
    }

toOffset :: ArticleDisplaySettings -> Offset
toOffset (ArticleDisplaySettings s) = offset s.pageNumber s.perPage

_pageNumber :: forall r a b. Forget r PageNumber a -> Forget r ArticleDisplaySettings b
_pageNumber = to (\(ArticleDisplaySettings s) -> s.pageNumber)

emptyPageNumber :: PageNumber
emptyPageNumber = PageNumber 0

noOffset :: Offset
noOffset = Offset 0

pageCount :: ArticleDisplaySettings -> Int
pageCount (ArticleDisplaySettings s) = let (PageNumber n) = computeLast s.articleCount s.perPage in n + 1

foldPages :: forall a. ArticleDisplaySettings -> a -> (a -> Page -> a) -> a
foldPages (ArticleDisplaySettings settings) accum = go 0 accum settings.pageNumber settings.perPage settings.articleCount
  where
  go i acc p1@(PageNumber pn) p2@(PerPage pp) a@(ArticleCount ac) f
    | i == last = f acc (mkPage i p2)
    | otherwise = go (i + 1) (f acc (mkPage i p2)) p1 p2 a f

  last = fromPageNumber $ computeLast settings.articleCount settings.perPage

  current = fromPageNumber settings.pageNumber

  mkPage i pp =
    let
      pn = PageNumber i
    in
      if i == current then
        CurrentPage pn
      else
        OtherPage pn (offset pn pp) (Distance $ abs $ current - i) (Distance $ if i < current then i else (last - i))

newtype Distance
  = Distance Int

derive instance newtypeDistance :: Newtype Distance _

data Page
  = CurrentPage PageNumber
  | OtherPage PageNumber Offset Distance Distance
