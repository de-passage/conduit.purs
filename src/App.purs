module App where

import Prelude
import Data.Either as E
import Data.GlobalState as GlobalState
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Data.User (User, deleteStoredUser, retrieveUser, storeUser)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Pages.Article as Pages.Article
import Pages.Authentication as Pages.Authentication
import Pages.Edition as Pages.Edition
import Pages.Home as Pages.Home
import Pages.Profile as Pages.Profile
import Pages.Settings as Pages.Settings
import Router (Route(..), route, routeWith404)
import Templates.Footer as Footer
import Templates.Navbar as Navbar

type State
  = GlobalState.State

data Action
  = Initialize
  | LogOut
  | LogIn User
  | Redirect String

data Query a
  = ChangeRoute String a

type ChildSlots
  = ( homepage :: Pages.Home.Slot Unit
    , showArticle :: Pages.Article.Slot Unit
    , profile :: Pages.Profile.Slot Unit
    , authentication :: Pages.Authentication.Slot Unit
    , settings :: Pages.Settings.Slot Unit
    )

type Input
  = String

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
              , initialize = Just Initialize
              }
    }

initialState :: Input -> State
initialState url = { currentRoute: initialRoute, currentUser: Nothing }
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
showPage r s = case r of
  Home -> home
  Login -> HH.slot _authentication unit Pages.Authentication.component Pages.Authentication.Login handleAuthenticationMessages
  Register -> HH.slot _authentication unit Pages.Authentication.component Pages.Authentication.Register handleAuthenticationMessages
  Settings -> authenticated settings home
  NewArticle -> Pages.Edition.render
  EditArticle _ -> Pages.Edition.render
  ShowArticle slug -> HH.slot _showArticle unit Pages.Article.component slug absurd
  Profile username -> HH.slot _profile unit Pages.Profile.component (Pages.Profile.Authored username) absurd
  Favorites username -> HH.slot _profile unit Pages.Profile.component (Pages.Profile.Favorited username) absurd
  NotFound url -> HH.div_ [ HH.text $ "Oops! It looks like the page you requested (" <> unwrap url <> ") doesn't exist!" ]
  where
  authenticated a b = case s.currentUser of
    Just user -> a user
    Nothing -> b

  settings user = HH.slot _settings unit Pages.Settings.component user handleSettingsMessages

  home = HH.slot _homePage unit Pages.Home.component s absurd

handleAction ∷ forall o m. MonadEffect m => Action → H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  Initialize -> do
    user <- H.liftEffect retrieveUser
    H.modify_ (_ { currentUser = user })
  LogOut -> do
    H.liftEffect deleteStoredUser
    H.modify_ (_ { currentUser = Nothing, currentRoute = Home })
  LogIn user -> do
    H.liftEffect $ storeUser user
    H.modify_ (_ { currentUser = Just user, currentRoute = Home })
  Redirect url -> pure unit

handleQuery :: forall o m a. MonadEffect m => Query a -> H.HalogenM State Action ChildSlots o m (Maybe a)
handleQuery = case _ of
  ChangeRoute msg a -> do
    E.either
      ( \s -> do
          liftEffect $ log s
          pure unit
      )
      identity
      (route (\r -> H.modify_ (_ { currentRoute = r })) msg)
    pure (Just a)

log' :: Route -> Effect Unit
log' = log <<< show

handleAuthenticationMessages :: Pages.Authentication.Output -> Maybe Action
handleAuthenticationMessages =
  Just
    <<< case _ of
        Pages.Authentication.LoginPerformed user -> LogIn user

handleSettingsMessages :: Pages.Settings.Output -> Maybe Action
handleSettingsMessages =
  Just
    <<< case _ of
        Pages.Settings.LogOutRequested -> LogOut
        Pages.Settings.UserUpdated user -> LogIn user
