module Pages.Profile where

import Prelude

import API as API
import Classes as C
import Control.Parallel (parSequence_)
import Data.Article (Article)
import Data.Const (Const)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.User (Profile, Username, fromImage)
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

data Input
  = Authored Username
  | Favorited Username

extract :: Input -> Username
extract (Authored u) = u
extract (Favorited u) = u

dispatch :: forall a. a -> a -> Input -> a
dispatch a _ (Authored _) = a
dispatch _ a (Favorited _) = a

type Slot
  = H.Slot Query Output

type State
  = { profile :: LoadState Profile
    , username :: Input
    , articles :: LoadState (Array Article)
    }

data Action
  = Init
  | Receive Input

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
  let
    showFavorites = dispatch false true state.username
    articleClass = if showFavorites then [ BS.navLink ] else [ BS.navLink, BS.active ]
    favoriteClass = if showFavorites then [ BS.navLink, BS.active ] else [ BS.navLink ]
  in
    case state.profile of
        Loading -> HH.div_ [ HH.text "Loading"]
        LoadError error -> HH.div [ HP.class_ BS.alertDanger ] [ HH.text error ]
        Loaded profile -> 
            HH.div [ HP.class_ C.profilePage ]
                [ HH.div [ HP.class_ C.userInfo ]
                    [ HH.div [ HP.class_ BS.container ]
                        [ HH.div [ HP.class_ BS.row ]
                            [ HH.div [ HP.classes [ C.colXs12, BS.colMd10, BS.offsetMd1 ] ]
                                [ HH.img [ HP.src $ fromImage profile.image, HP.class_ C.userImg ]
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
                                        [ HH.a [ HP.classes articleClass, HP.href (profileUrl profile.username) ] [ HH.text "My Articles" ]
                                        ]
                                    , HH.li [ HP.class_ BS.navItem ]
                                        [ HH.a [ HP.classes favoriteClass, HP.href (favoritesUrl profile.username) ] [ HH.text "Favored Articles" ]
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
    H.modify_ (_ { username = username })
  where
  setArticles c u v s = s { articles = v, username = c u}
  loadProfile username = load (API.getProfile (extract username) Nothing) (\v -> _ { profile = v })
  loadArticles (Authored username) = load (API.getUserArticles username Nothing) $ setArticles Authored username
  loadArticles (Favorited username) = load (API.getFavorites username Nothing) $ setArticles Favorited username