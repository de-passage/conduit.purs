module Router
  ( Route(..)
  , route
  , homeUrl
  , settingsUrl
  , profileUrl
  , loginUrl
  , registerUrl
  , favoritesUrl
  , newArticleUrl
  , editArticleUrl
  , showArticleUrl
  ) where

import Prelude
import Data.Article (Slug(..))
import Data.Either (Either)
import Data.Foldable (oneOf)
import Data.Maybe (fromMaybe)
import Data.User (Username(..))
import Global (decodeURIComponent)
import Routing.Match (Match, end, lit, runMatch, str, root)
import Routing.Parser as Parser

data Route
  = Home
  | Login
  | Register
  | Settings
  | NewArticle
  | EditArticle Slug
  | ShowArticle Slug
  | Profile Username
  | Favorites Username

instance showRoute :: Show Route where
  show Home = "Home"
  show Register = "Register"
  show Settings = "Settings"
  show Login = "Login"
  show NewArticle = "NewArticle"
  show (EditArticle _) = "EditArticle"
  show (ShowArticle _) = "ShowArticle"
  show (Profile _) = "Profile"
  show (Favorites _) = "Favorites"

home :: Match Route
home = Home <$ end

homeUrl :: String
homeUrl = "#/"

login :: Match Route
login = Login <$ lit "login" <* end

loginUrl :: String
loginUrl = "#/login/"

register :: Match Route
register = Register <$ lit "register" <* end

registerUrl :: String
registerUrl = "#/register/"

settings :: Match Route
settings = Settings <$ lit "settings" <* end

settingsUrl :: String
settingsUrl = "#/settings/"

newArticle :: Match Route
newArticle = NewArticle <$ lit "editor" <* end

newArticleUrl :: String
newArticleUrl = "#/editor/"

editArticle :: Match Route
editArticle = EditArticle <$> (lit "editor" *> slug) <* end

editArticleUrl :: Slug -> String
editArticleUrl (Slug s) = "#/editor/" <> s <> "/"

showArticle :: Match Route
showArticle = ShowArticle <$> (lit "article" *> slug) <* end

showArticleUrl :: Slug -> String
showArticleUrl (Slug s) = "#/article/" <> s <> "/"

profile :: Match Route
profile = Profile <$> (lit "profile" *> username) <* end

profileUrl :: Username -> String
profileUrl (Username user) = "#/profile/" <> user <> "/"

favorites :: Match Route
favorites = Favorites <$> (lit "profile" *> username) <* lit "favorites" <* end

favoritesUrl :: Username -> String
favoritesUrl (Username user) = "#/profile/" <> user <> "/favorites/"

slug :: Match Slug
slug = Slug <$> str

username :: Match Username
username = Username <$> str

router :: Match Route
router =
  root
    *> oneOf
        [ home
        , login
        , register
        , settings
        , newArticle
        , editArticle
        , showArticle
        , profile
        , favorites
        ]

route :: forall a. (Route -> a) -> String -> Either String a
route f r =
  Parser.parse (decodeURIComponent >>> fromMaybe "") r
    # runMatch router
    # map f