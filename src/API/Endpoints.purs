module Data.Endpoint
  ( articles
  , article
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
  , Endpoint
  ) where

import Data.Article (Slug)
import Data.Tag (Tag)
import Data.User (Username)
import Prelude (class Show, (<>), show, ($))

newtype Endpoint = Endpoint String

instance showEndpoint :: Show Endpoint where 
    show (Endpoint url) = url

class ShowUnquoted m where
  showUnquoted :: m -> String

instance showUnquotedEndpoint :: ShowUnquoted Endpoint where
  showUnquoted = show

instance showUnquotedUsername :: ShowUnquoted Username where
  showUnquoted = show

instance showUnquotedSlug :: ShowUnquoted Slug where
  showUnquoted = show

instance showUnquotedTag :: ShowUnquoted Tag where
  showUnquoted = show

instance showUnquotedString :: ShowUnquoted String where
  showUnquoted s = s

combine :: forall a b. ShowUnquoted a => ShowUnquoted b => a -> b -> Endpoint
combine a b = Endpoint $ showUnquoted a <> showUnquoted b
infixl 4 combine as <.>

root :: Endpoint
root = Endpoint "https://conduit.productionready.io/api/"

articles :: Endpoint
articles = root <.> "articles/"

article :: Slug -> Endpoint
article s = articles <.> s <.> "/"

profile :: Username -> Endpoint
profile u = root <.> "profiles/" <.> u <.> "/"

follow :: Username -> Endpoint
follow u = profile u <.> "follow/"

tags :: Endpoint
tags = root <.> "tags/"

comments :: Slug -> Endpoint
comments slug = article slug <.> "comments/"

users :: Endpoint
users = root <.> "users/"

feed :: Endpoint
feed = articles <.> "feed/"

login :: Endpoint
login = users <.> "login/"

userArticles :: Username -> Endpoint
userArticles u = articles <.> "?author=" <.> u

taggedArticles :: Tag -> Endpoint
taggedArticles t = articles <.> "?tag=" <.> t

favorites :: Username -> Endpoint
favorites u = articles <.> "?favorited=" <.> u
