module App where

import Prelude

import Data.Either as E
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Console (log)
import Footer as Footer
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Navbar as Navbar
import Pages.Article as Pages.Article
import Pages.Authentication as Pages.Authentication
import Pages.Edition as Pages.Edition
import Pages.Home as Pages.Home
import Pages.Profile as Pages.Profile
import Pages.Settings as Pages.Settings
import Router (Route(..), route)

type State
  = { currentRoute :: Route }

data Action
  = Unit

data Query a
  = ChangeRoute String a

type ChildSlots = ( homepage :: Pages.Home.Slot Unit )

_homePage :: SProxy "homepage"
_homePage = SProxy

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
  ShowArticle _ -> Pages.Article.render
  Profile _ -> Pages.Profile.render
  Favorites _ -> Pages.Profile.render

handleAction ∷ forall o m. Action → H.HalogenM State Action ChildSlots o m Unit
handleAction _ = pure unit

handleQuery :: forall o m a. MonadEffect m => Query a -> H.HalogenM State Action ChildSlots o m (Maybe a)
handleQuery = case _ of
  ChangeRoute msg a -> do
    liftEffect $ log msg
    E.either
      ( \s -> do
          liftEffect $ log s
          pure unit
      )
      identity
      (route (\r -> H.modify_ (_ { currentRoute = r })) msg)
    pure (Just a)
