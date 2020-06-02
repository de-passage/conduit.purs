module Pages.Profile where

import Prelude

import API as API
import Classes as C
import Control.Parallel (parSequence_)
import Data.Article (Article)
import Data.Const (Const)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.User (Username, Profile)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as BS
import LoadState (LoadState(..), load)
import Router (favoritesUrl, profileUrl)
import Templates.ArticlePreview as ArticlePreview

type Query
  = Const Void

type Output
  = Void

type Input
  = Username

type Slot
  = H.Slot Query Output

type State
  = { profile :: LoadState Profile
    , username :: Username
    , articles :: LoadState (Array Article)
    }

data Action
  = Init
  | Receive Username

type ChildSlots
  = ()

component :: forall m. MonadAff m => H.Component HH.HTML Query Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , receive = Just <<< Receive
              , initialize = Just Init
              }
    }

initialState :: Input -> State
initialState username = { profile: Loading, username, articles : Loading }

render :: forall m. State -> HH.ComponentHTML Action ChildSlots m
render state =
    case state.profile of
        Loading -> HH.div_ [ HH.text "Loading"]
        LoadError error -> HH.div [ HP.class_ BS.alertDanger ] [ HH.text error ]
        Loaded profile -> 
            HH.div [ HP.class_ C.profilePage ]
                [ HH.div [ HP.class_ C.userInfo ]
                    [ HH.div [ HP.class_ BS.container ]
                        [ HH.div [ HP.class_ BS.row ]
                            [ HH.div [ HP.classes [ C.colXs12, BS.colMd10, BS.offsetMd1 ] ]
                                [ HH.img [ HP.src profile.image, HP.class_ C.userImg ]
                                , HH.h4_ [ HH.text $ unwrap profile.username ]
                                , HH.p_ [ HH.text $ fromMaybe "" profile.bio ]
                                , HH.button [ HP.classes [ BS.btn, BS.btnOutlineSecondary, C.actionBtn ] ]
                                    [ HH.i [ HP.class_ C.ionPlusRound ] []
                                    , HH.text $ " Follow " <> unwrap profile.username
                                    ]
                                ]
                            ]
                        ]
                    ]
                , HH.div [ HP.class_ BS.container ]
                    [ HH.div [ HP.class_ BS.row ]
                        [ HH.div [ HP.classes [ C.colXs12, BS.colMd10, BS.offsetMd1 ] ]
                            [ HH.div [ HP.class_ C.articleToggle ]
                                ([ HH.ul [ HP.classes [ BS.nav, BS.navPills, C.outlineActive ] ]
                                    [ HH.li [ HP.class_ BS.navItem ]
                                        [ HH.a [ HP.classes [ BS.navLink, BS.active ], HP.href (profileUrl profile.username) ] [ HH.text "My Articles" ]
                                        ]
                                    , HH.li [ HP.class_ BS.navItem ]
                                        [ HH.a [ HP.classes [ BS.navLink ], HP.href (favoritesUrl profile.username) ] [ HH.text "Favored Articles" ]
                                        ]
                                    ]
                                ]
                                <> showArticles state.articles)
                            ]
                        ]
                    ]
                ]
    where 
        showArticles =
            case _ of
                Loading -> [ HH.div_ [ HH.text "Loading"] ]
                LoadError err -> [ HH.div [HP.classes [ BS.alert, BS.alertDanger ] ] [ HH.text err] ]
                Loaded articles -> map ArticlePreview.render articles

handleAction ∷
  forall o m.
  MonadAff m =>
  Action ->
  H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  Init -> do
    state <- H.get
    handleAction (Receive state.username)
  Receive username -> do
    parSequence_ [ loadProfile username, loadArticles username ]
  where
  loadProfile username = load (API.getProfile username) (\v -> _ { profile = v })
  loadArticles username = load (API.getUserArticles username) (\v -> _ { articles = v })