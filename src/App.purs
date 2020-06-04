module App where

import Prelude

import Data.Either as E
import Data.GlobalState as GlobalState
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
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
  = Unit

data Query a
  = ChangeRoute String a

type ChildSlots
  = ( homepage :: Pages.Home.Slot Unit
    , showArticle :: Pages.Article.Slot Unit
    , profile :: Pages.Profile.Slot Unit
    , authentication :: Pages.Authentication.Slot Unit
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

component :: forall o m. MonadAff m => H.Component HH.HTML Query Input o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, handleQuery = handleQuery }
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
    [ Navbar.render
    , showPage state.currentRoute state
    , Footer.render
    ]

showPage :: forall m. MonadAff m => Route -> State -> H.ComponentHTML Action ChildSlots m
showPage r s = case r of
  Home -> HH.slot _homePage unit Pages.Home.component s absurd
  Login -> HH.slot _authentication unit Pages.Authentication.component Pages.Authentication.Login absurd
  Register -> HH.slot _authentication unit Pages.Authentication.component Pages.Authentication.Register absurd
  Settings -> Pages.Settings.render
  NewArticle -> Pages.Edition.render
  EditArticle _ -> Pages.Edition.render
  ShowArticle slug -> HH.slot _showArticle unit Pages.Article.component slug absurd
  Profile username -> HH.slot _profile unit Pages.Profile.component (Pages.Profile.Authored username) absurd
  Favorites username -> HH.slot _profile unit Pages.Profile.component (Pages.Profile.Favorited username) absurd
  NotFound url -> HH.div_ [ HH.text $ "Oops! It looks like the page you requested (" <> unwrap url <> ") doesn't exist!" ]

handleAction ∷ forall o m. Action → H.HalogenM State Action ChildSlots o m Unit
handleAction _ = pure unit

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
log' =
  log <<< show