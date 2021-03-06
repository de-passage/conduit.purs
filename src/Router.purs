module Router
  ( Route(..)
  , route
  , devToolsUrl
  , homeUrl
  , settingsUrl
  , profileUrl
  , loginUrl
  , registerUrl
  , favoritesUrl
  , newArticleUrl
  , editArticleUrl
  , showArticleUrl
  , routeWith404
  , redirect
  ) where

import Prelude
import Data.Article (Slug(..))
import Data.Either (Either, either)
import Data.Foldable (oneOf)
import Data.Maybe (fromMaybe)
import Data.Newtype (unwrap)
import Data.User (Username(..))
import Effect (Effect)
import Global (decodeURIComponent)
import Routing.Match (Match, end, lit, runMatch, str, root)
import Routing.Parser as Parser
import Web.HTML (window) as DOM
import Web.HTML.Location (setHash) as DOM
import Web.HTML.Window (location) as DOM

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
  | NotFound String
  | DevTools

instance showRoute :: Show Route where
  show Home = "Home"
  show Register = "Register"
  show Settings = "Settings"
  show Login = "Login"
  show NewArticle = "NewArticle"
  show (EditArticle s) = "EditArticle " <> unwrap s
  show (ShowArticle s) = "ShowArticle " <> unwrap s
  show (Profile u) = "Profile " <> unwrap u
  show (Favorites u) = "Favorites " <> unwrap u
  show DevTools = "DevTool"
  show (NotFound url) = "Url not found: " <> url

home :: Match Route
home = Home <$ end

homeUrl :: String
homeUrl = "#/"

devToolsUrl :: String
devToolsUrl = "#/dev/"

devTools :: Match Route
devTools = DevTools <$ lit "dev" <* end

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
editArticleUrl s = "#/editor/" <> show s <> "/"

showArticle :: Match Route
showArticle = ShowArticle <$> (lit "article" *> slug) <* end

showArticleUrl :: Slug -> String
showArticleUrl s = "#/article/" <> show s <> "/"

profile :: Match Route
profile = Profile <$> (lit "profile" *> username) <* end

profileUrl :: Username -> String
profileUrl user = "#/profile/" <> show user <> "/"

favorites :: Match Route
favorites = Favorites <$> (lit "profile" *> username) <* lit "favorites" <* end

favoritesUrl :: Username -> String
favoritesUrl user = "#/profile/" <> show user <> "/favorites/"

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
        , devTools
        ]

route :: forall a. (Route -> a) -> String -> Either String a
route f r =
  Parser.parse (decodeURIComponent >>> fromMaybe "") r
    # runMatch router
    # map f

routeWith404 :: forall a. (Route -> a) -> String -> a
routeWith404 f r =
  route f r
    # either (\_ -> f (NotFound r)) identity

redirect :: String -> Effect Unit
redirect url = do
  window <- DOM.window
  loc <- DOM.location window
  DOM.setHash url loc
