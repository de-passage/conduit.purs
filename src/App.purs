module App where

import Prelude
import API.Url as Urls
import Data.Either as E
import Data.GlobalState as GlobalState
import Data.Maybe (Maybe(..))
import Data.Root (Root)
import Data.Symbol (SProxy(..))
import Data.User (User)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Pages.Article as Pages.Article
import Pages.Authentication as Pages.Authentication
import Pages.DevTools as Pages.DevTools
import Pages.Edition as Pages.Edition
import Pages.Home as Pages.Home
import Pages.Profile as Pages.Profile
import Pages.Settings as Pages.Settings
import Router (Route(..), homeUrl, profileUrl, redirect, route, routeWith404, showArticleUrl)
import Storage as S
import Templates.Footer as Footer
import Templates.Navbar as Navbar
import Utils as Utils

type State
  = GlobalState.State

data Action
  = LogOut
  | LogIn User
  | Redirect String
  | UpdateUser User
  | ChangeUrls Root

data Query a
  = ChangeRoute String a

type ChildSlots
  = ( homepage :: Pages.Home.Slot Unit
    , showArticle :: Pages.Article.Slot Unit
    , profile :: Pages.Profile.Slot Unit
    , authentication :: Pages.Authentication.Slot Unit
    , settings :: Pages.Settings.Slot Unit
    , newArticle :: Pages.Edition.Slot Unit
    , editArticle :: Pages.Edition.Slot Unit
    , devTools :: Pages.DevTools.Slot Unit
    )

type Input
  = { url :: String
    , user :: Maybe User
    , repo :: Urls.UrlRepository
    }

_homePage :: SProxy "homepage"
_homePage = SProxy

_showArticle :: SProxy "showArticle"
_showArticle = SProxy

_profile :: SProxy "profile"
_profile = SProxy

_authentication :: SProxy "authentication"
_authentication = SProxy

_settings :: SProxy "settings"
_settings = SProxy

_newArticle :: SProxy "newArticle"
_newArticle = SProxy

_editArticle :: SProxy "editArticle"
_editArticle = SProxy

_devTools :: SProxy "devTools"
_devTools = SProxy

component :: forall o m. MonadAff m => H.Component HH.HTML Query Input o m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , handleQuery = handleQuery
              }
    }

initialState :: Input -> State
initialState { url, user, repo } = { currentRoute: initialRoute, currentUser: user, urls: repo }
  where
  initialRoute :: Route
  initialRoute =
    if url == "" then
      Home
    else
      routeWith404 identity url

render :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
render state =
  HH.div_
    [ Navbar.render state.currentUser
    , showPage state.currentRoute state
    , Footer.render
    ]

showPage :: forall m. MonadAff m => Route -> State -> H.ComponentHTML Action ChildSlots m
showPage r s@{ urls, currentUser, currentRoute } = case r of
  Home -> home
  Login ->
    HH.slot _authentication unit Pages.Authentication.component
      { urls, action: Pages.Authentication.Login }
      handleAuthenticationMessages
  Register ->
    HH.slot _authentication unit Pages.Authentication.component
      { urls, action: Pages.Authentication.Register }
      handleAuthenticationMessages
  Settings -> authenticated settings home
  NewArticle -> authenticated newArticle home
  EditArticle slug -> authenticated (editArticle slug) home
  ShowArticle slug ->
    HH.slot _showArticle unit Pages.Article.component
      { slug, currentUser, urls }
      handleArticleMessages
  Profile username ->
    HH.slot _profile unit Pages.Profile.component
      { page: (Pages.Profile.Authored username)
      , currentUser
      , urls
      }
      absurd
  Favorites username ->
    HH.slot _profile unit Pages.Profile.component
      { page: (Pages.Profile.Favorited username)
      , currentUser
      , urls
      }
      absurd
  NotFound url -> HH.div_ [ HH.text $ "Oops! It looks like the page you requested (" <> url <> ") doesn't exist!" ]
  DevTools -> HH.slot _devTools unit Pages.DevTools.component { urls } handleDevToolMessages
  where
  authenticated a b = case currentUser of
    Just user -> a user
    Nothing -> b

  settings user =
    HH.slot _settings unit Pages.Settings.component
      { currentUser: user, urls }
      handleSettingsMessages

  home = HH.slot _homePage unit Pages.Home.component { urls, currentUser } absurd

  newArticle user =
    HH.slot _newArticle
      unit
      Pages.Edition.component
      { currentAction: Pages.Edition.New, currentUser: user, urls }
      handleEditionMessages

  editArticle slug user =
    HH.slot
      _editArticle
      unit
      Pages.Edition.component
      { currentAction: Pages.Edition.Edit slug, currentUser: user, urls }
      handleEditionMessages

handleAction ∷ forall o m. MonadEffect m => Action → H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  LogOut -> do
    H.liftEffect S.deleteStoredUser
    H.modify_ (_ { currentUser = Nothing })
    handleAction $ Redirect homeUrl
  LogIn user -> do
    H.liftEffect $ S.storeUser user
    H.modify_ (_ { currentUser = Just user })
    handleAction $ Redirect homeUrl
  UpdateUser user -> do
    H.modify_ _ { currentUser = Just user }
    handleAction $ Redirect (profileUrl user.username)
  Redirect url -> H.liftEffect $ redirect url
  ChangeUrls root -> do
    let
      newRepo = Urls.repository root
    H.liftEffect do
      S.saveRepository newRepo
      S.deleteStoredUser
    H.modify_ _ { currentUser = Nothing, urls = newRepo }

handleQuery :: forall o m a. MonadEffect m => Query a -> H.HalogenM State Action ChildSlots o m (Maybe a)
handleQuery = case _ of
  ChangeRoute msg a -> do
    E.either
      ( \s -> do
          Utils.log s
          pure unit
      )
      identity
      (route (\r -> H.modify_ (_ { currentRoute = r })) msg)
    pure (Just a)

handleAuthenticationMessages :: Pages.Authentication.Output -> Maybe Action
handleAuthenticationMessages =
  Just
    <<< case _ of
        Pages.Authentication.LoginPerformed user -> LogIn user

handleEditionMessages :: Pages.Edition.Output -> Maybe Action
handleEditionMessages =
  Just
    <<< case _ of
        Pages.Edition.Redirect slug -> Redirect (showArticleUrl slug)

handleArticleMessages :: Pages.Article.Output -> Maybe Action
handleArticleMessages =
  Just
    <<< case _ of
        Pages.Article.Redirect url -> Redirect url

handleSettingsMessages :: Pages.Settings.Output -> Maybe Action
handleSettingsMessages =
  Just
    <<< case _ of
        Pages.Settings.LogOutRequested -> LogOut
        Pages.Settings.UserUpdated user -> UpdateUser user

handleDevToolMessages :: Pages.DevTools.Output -> Maybe Action
handleDevToolMessages =
  Just
    <<< case _ of
        Pages.DevTools.RootChanged root -> ChangeUrls root
