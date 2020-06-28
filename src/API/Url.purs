module API.Url
  ( defaultArticleOptions
  , defaultArticleLimit
  , repository
  , Url
  , ArticleOptions
  , ArticleLimit
  , Limit
  , UrlRepository
  ) where

import Data.Array (catMaybes, intercalate, length)
import Data.Article (Offset, PerPage, Slug)
import Data.Comment (CommentId)
import Data.Maybe (Maybe(..))
import Data.Root (Root)
import Data.Tag (Tag)
import Data.User (Username)
import Prelude (class Show, show, ($), (<#>), (<>), (>))

newtype Url
  = Url String

instance showUrl :: Show Url where
  show (Url url) = url

class ShowUnquoted m where
  showUnquoted :: m -> String

instance showUnquotedString :: ShowUnquoted String where
  showUnquoted s = s
else instance showUnquotedShow :: Show s => ShowUnquoted s where
  showUnquoted = show

combine :: forall a b. ShowUnquoted a => ShowUnquoted b => a -> b -> Url
combine a b = Url $ showUnquoted a <> showUnquoted b

infixl 4 combine as <.>

type UrlRepository
  = { root :: Root
    , article :: Slug -> Url
    , articles :: ArticleOptions -> Url
    , allArticles :: Url
    , favorite :: Slug -> Url
    , profile :: Username -> Url
    , follow :: Username -> Url
    , tags :: Url
    , comments :: Slug -> Url
    , comment :: Slug -> CommentId -> Url
    , users :: Url
    , currentUser :: Url
    , feed :: Url
    , limitedFeed :: ArticleLimit -> Url
    , login :: Url
    , userArticles :: Username -> Url
    , taggedArticles :: Tag -> Url
    , favorites :: Username -> Url
    , register :: Url
    }

repository :: Root -> UrlRepository
repository root =
  { root
  , article
  , articles
  , allArticles
  , profile
  , follow
  , tags
  , comments
  , comment
  , users
  , currentUser
  , feed
  , limitedFeed
  , login
  , userArticles
  , taggedArticles
  , favorites
  , favorite
  , register
  }
  where
  allArticles :: Url
  allArticles = root <.> "articles/"

  article :: Slug -> Url
  article s = allArticles <.> s <.> "/"

  favorite :: Slug -> Url
  favorite s = article s <.> "favorite/"

  profile :: Username -> Url
  profile u = root <.> "profiles/" <.> u <.> "/"

  follow :: Username -> Url
  follow u = profile u <.> "follow/"

  tags :: Url
  tags = root <.> "tags/"

  comments :: Slug -> Url
  comments slug = article slug <.> "comments/"

  comment :: Slug -> CommentId -> Url
  comment slug id = comments slug <.> id <.> "/"

  users :: Url
  users = root <.> "users/"

  currentUser :: Url
  currentUser = root <.> "user/"

  feed :: Url
  feed = allArticles <.> "feed/"

  limitedFeed :: ArticleLimit -> Url
  limitedFeed opts =
    if length prefixed > 0 then
      feed <.> "?" <.> options
    else
      feed
    where
    options = intercalate "&" prefixed

    prefixed =
      catMaybes
        [ prefix "limit" opts.limit
        , prefix "offset" opts.offset
        ]

  login :: Url
  login = users <.> "login/"

  register :: Url
  register = users

  articles :: ArticleOptions -> Url
  articles opts =
    if length prefixed > 0 then
      allArticles <.> "?" <.> options
    else
      allArticles
    where
    options = intercalate "&" prefixed

    prefixed =
      catMaybes
        [ prefix "author" opts.author
        , prefix "tag" opts.tag
        , prefix "favorited" opts.favorited
        , prefix "limit" opts.limit
        , prefix "offset" opts.offset
        ]

  prefix :: forall v. Show v => String -> Maybe v -> Maybe String
  prefix name value = value <#> \v -> name <> "=" <> show v

  userArticles :: Username -> Url
  userArticles u = articles $ defaultArticleOptions { author = Just u }

  taggedArticles :: Tag -> Url
  taggedArticles t = articles $ defaultArticleOptions { tag = Just t }

  favorites :: Username -> Url
  favorites u = articles $ defaultArticleOptions { favorited = Just u }

type Limit
  = ( limit :: Maybe PerPage, offset :: Maybe Offset )

type ArticleLimit
  = Record Limit

type ArticleOptions
  = { author :: Maybe Username
    , tag :: Maybe Tag
    , favorited :: Maybe Username
    | Limit
    }

defaultArticleOptions :: ArticleOptions
defaultArticleOptions =
  { author: Nothing
  , tag: Nothing
  , favorited: Nothing
  , limit: Nothing
  , offset: Nothing
  }

defaultArticleLimit :: ArticleLimit
defaultArticleLimit =
  { limit: Nothing
  , offset: Nothing
  }
