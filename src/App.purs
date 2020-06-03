module App where

import Prelude

import Data.Either as E
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
import Pages.Profile (Input(..))
import Pages.Profile as Pages.Profile
import Pages.Settings as Pages.Settings
import Router (Route(..), route)
import Templates.Footer as Footer
import Templates.Navbar as Navbar

type State
  = { currentRoute :: Route }

data Action
  = Unit

data Query a
  = ChangeRoute String a

type ChildSlots
  = ( homepage :: Pages.Home.Slot Unit
    , showArticle :: Pages.Article.Slot Unit
    , profile :: Pages.Profile.Slot Unit
    )

_homePage :: SProxy "homepage"
_homePage = SProxy

_showArticle :: SProxy "showArticle"
_showArticle = SProxy

_profile :: SProxy "profile"
_profile = SProxy

component :: forall i o m. MonadAff m => H.Component HH.HTML Query i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, handleQuery = handleQuery }
    }

initialState :: forall i. i -> State
initialState _ = { currentRoute: Home }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
render state =
  HH.div_
    [ Navbar.render
    , showPage state.currentRoute state
    , Footer.render
    ]

showPage :: forall m. MonadAff m => Route -> State -> H.ComponentHTML Action ChildSlots m
showPage r s = case r of
  Home -> HH.slot _homePage unit Pages.Home.component unit absurd
  Login -> Pages.Authentication.render
  Register -> Pages.Authentication.render
  Settings -> Pages.Settings.render
  NewArticle -> Pages.Edition.render
  EditArticle _ -> Pages.Edition.render
  ShowArticle slug -> HH.slot _showArticle unit Pages.Article.component slug absurd
  Profile username -> HH.slot _profile unit Pages.Profile.component (Authored username) absurd
  Favorites username -> HH.slot _profile unit Pages.Profile.component (Favorited username) absurd

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
  log
    <<< case _ of
        Home -> "Home"
        Login -> "Login"
        Register -> "Register"
        Settings -> "Settings"
        NewArticle -> "New article"
        EditArticle slug -> "Edit " <> unwrap slug
        ShowArticle slug -> "Show " <> unwrap slug
        Profile username -> "Profile " <> unwrap username
        Favorites username -> "Favorites " <> unwrap username
