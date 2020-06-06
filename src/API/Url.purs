module API.Url
  ( articles
  , article
  , allArticles
  , defaultArticleOptions
  , profile
  , follow
  , tags
  , comments
  , users
  , feed
  , login
  , userArticles
  , taggedArticles
  , favorites
  , register
  , Url
  , ArticleOptions
  ) where

import Data.Array (catMaybes, intercalate)
import Data.Article (Slug)
import Data.Maybe (Maybe(..))
import Data.Tag (Tag)
import Data.User (Username)
import Prelude (class Show, show, (#), ($), (<#>), (<>))

newtype Url
  = Url String

instance showUrl :: Show Url where
  show (Url url) = url

class ShowUnquoted m where
  showUnquoted :: m -> String

instance showUnquotedUrl :: ShowUnquoted Url where
  showUnquoted = show

instance showUnquotedUsername :: ShowUnquoted Username where
  showUnquoted = show

instance showUnquotedSlug :: ShowUnquoted Slug where
  showUnquoted = show

instance showUnquotedTag :: ShowUnquoted Tag where
  showUnquoted = show

instance showUnquotedString :: ShowUnquoted String where
  showUnquoted s = s

combine :: forall a b. ShowUnquoted a => ShowUnquoted b => a -> b -> Url
combine a b = Url $ showUnquoted a <> showUnquoted b

infixl 4 combine as <.>

root :: Url
root = Url "https://conduit.productionready.io/api/"

allArticles :: Url
allArticles = root <.> "articles/"

article :: Slug -> Url
article s = allArticles <.> s <.> "/"

profile :: Username -> Url
profile u = root <.> "profiles/" <.> u <.> "/"

follow :: Username -> Url
follow u = profile u <.> "follow/"

tags :: Url
tags = root <.> "tags/"

comments :: Slug -> Url
comments slug = article slug <.> "comments/"

users :: Url
users = root <.> "users/"

feed :: Url
feed = allArticles <.> "feed/"

login :: Url
login = users <.> "login/"

register :: Url
register = users

type ArticleOptions
  = { author :: Maybe Username
    , tag :: Maybe Tag
    , favorited :: Maybe Username
    , limit :: Maybe Int
    , offset :: Maybe Int
    }

defaultArticleOptions :: ArticleOptions
defaultArticleOptions =
  { author: Nothing
  , tag: Nothing
  , favorited: Nothing
  , limit: Nothing
  , offset: Nothing
  }

articles :: ArticleOptions -> Url
articles opts = allArticles <.> "?" <.> options
  where
    options = catMaybes prefixed # intercalate ";"
    prefixed = [
      prefix "author" opts.author
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
