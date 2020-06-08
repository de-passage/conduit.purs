module Pages.Profile where

import Prelude

import API as API
import Classes as C
import Control.Parallel (parSequence_)
import Data.Article (Article)
import Data.Const (Const)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.User (Profile, Username, User, fromImage)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as BS
import LoadState (LoadState(..), load)
import Router (favoritesUrl, profileUrl)
import Templates.ArticlePreview as ArticlePreview
import Utils as Utils
import Web.Event.Internal.Types (Event)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

type Query
  = Const Void

type Output
  = Void

data SubPage
  = Authored Username
  | Favorited Username

type Input
  = { page :: SubPage
    , currentUser :: Maybe User
    }

extract :: SubPage -> Username
extract (Authored u) = u

extract (Favorited u) = u

dispatch :: forall a. a -> a -> SubPage -> a
dispatch a _ (Authored _) = a

dispatch _ a (Favorited _) = a

type Slot
  = H.Slot Query Output

type State
  = { profile :: LoadState Profile
    , page :: SubPage
    , articles :: LoadState (Array Article)
    , currentUser :: Maybe User
    }

data Action
  = Init
  | Receive Input
  | PreventDefault Event (Maybe Action)
  | FavoritedButtonClicked Article
  | FollowButtonClicked Profile

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
initialState { page, currentUser } =
  { profile: Loading
  , page
  , articles: Loading
  , currentUser: currentUser
  }

render :: forall m. State -> HH.ComponentHTML Action ChildSlots m
render state =
  let
    showFavorites = dispatch false true state.page

    articleClass = if showFavorites then [ BS.navLink ] else [ BS.navLink, BS.active ]

    favoriteClass = if showFavorites then [ BS.navLink, BS.active ] else [ BS.navLink ]

    followButton profile =
      HH.button
        [ HP.classes
            [ BS.btn
            , if profile.following then BS.btnOutlineSecondary else BS.btnOutlinePrimary
            , C.actionBtn
            ]
        , HE.onClick $ preventDefault $ FollowButtonClicked profile
        ]
        [ HH.i [ HP.class_ C.ionPlusRound ] []
        , HH.text $ (if profile.following then " Unfollow " else " Follow ") <> unwrap profile.username
        ]
  in
    case state.profile of
      Loading -> HH.div_ [ HH.text "Loading" ]
      LoadError error -> HH.div [ HP.class_ BS.alertDanger ] [ Utils.errorDisplay error ]
      Loaded profile ->
        HH.div [ HP.class_ C.profilePage ]
          [ HH.div [ HP.class_ C.userInfo ]
              [ HH.div [ HP.class_ BS.container ]
                  [ HH.div [ HP.class_ BS.row ]
                      [ HH.div [ HP.classes [ C.colXs12, BS.colMd10, BS.offsetMd1 ] ]
                          [ HH.img [ HP.src $ fromImage profile.image, HP.class_ C.userImg ]
                          , HH.h4_ [ HH.text $ unwrap profile.username ]
                          , HH.p_ [ HH.text $ fromMaybe "" profile.bio ]
                          , ( state.currentUser
                                # maybe (const $ HH.div_ [])
                                    (\u -> if u.username == profile.username then const $ HH.div_ [] else followButton)
                            )
                              $ profile
                          ]
                      ]
                  ]
              ]
          , HH.div [ HP.class_ BS.container ]
              [ HH.div [ HP.class_ BS.row ]
                  [ HH.div [ HP.classes [ C.colXs12, BS.colMd10, BS.offsetMd1 ] ]
                      [ HH.div [ HP.class_ C.articleToggle ]
                          ( [ HH.ul [ HP.classes [ BS.nav, BS.navPills, C.outlineActive ] ]
                                [ HH.li [ HP.class_ BS.navItem ]
                                    [ HH.a [ HP.classes articleClass, HP.href (profileUrl profile.username) ] [ HH.text "My Articles" ]
                                    ]
                                , HH.li [ HP.class_ BS.navItem ]
                                    [ HH.a [ HP.classes favoriteClass, HP.href (favoritesUrl profile.username) ] [ HH.text "Favored Articles" ]
                                    ]
                                ]
                            ]
                              <> showArticles state.articles
                          )
                      ]
                  ]
              ]
          ]
  where
  showArticles = case _ of
    Loading -> [ HH.div_ [ HH.text "Loading" ] ]
    LoadError err -> [ HH.div [ HP.classes [ BS.alert, BS.alertDanger ] ] [ Utils.errorDisplay err ] ]
    Loaded articles -> map (ArticlePreview.render <*> preventDefault <<< FavoritedButtonClicked) articles

  preventDefault :: Action -> MouseEvent -> Maybe Action
  preventDefault action event = Just $ PreventDefault (toEvent event) $ Just action

handleAction ∷
  forall m.
  MonadAff m =>
  Action ->
  H.HalogenM State Action ChildSlots Output m Unit
handleAction = case _ of
  Init -> do
    state <- H.get
    handleAction (Receive { page: state.page, currentUser: state.currentUser })
  Receive { page, currentUser } -> do
    let
      token = currentUser <#> _.token
    parSequence_ [ loadProfile page token, loadArticles page token ]
    H.modify_ (_ { page = page })
  FavoritedButtonClicked article -> do
    user <- H.gets _.currentUser
    let
      token = user <#> _.token
    token
      # maybe (pure unit) \tok ->
          Utils.favorite article tok (\art -> _ { articles = art }) _.articles
  FollowButtonClicked profile -> do
    muser <- H.gets _.currentUser
    muser
      # maybe (pure unit) \user ->
          Utils.follow profile user.token (\prof -> _ { profile = prof })
  PreventDefault event action -> Utils.preventDefault event action handleAction
  where
  setArticles c u v s = s { articles = v, page = c u }

  loadProfile username token = load (API.getProfile (extract username) token) (\v -> _ { profile = v })

  loadArticles (Authored username) token = load (API.getUserArticles username token) $ setArticles Authored username

  loadArticles (Favorited username) token = load (API.getFavorites username token) $ setArticles Favorited username
