module Pages.Profile where

import Prelude
import API as API
import Classes as C
import Control.Parallel (parSequence_)
import Data.Article (Article, ArticleList)
import Data.Const (Const)
import Data.GlobalState (WithCommon)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.User (Profile, Username, fromImage)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as BS
import LoadState (LoadState(..), load)
import Router (favoritesUrl, loginUrl, profileUrl)
import Templates.ArticlePreview as ArticlePreview
import Utils as Utils
import Web.Event.Internal.Types (Event)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

type Query
  = Const Void

data Output
  = Redirect String

data SubPage
  = Authored Username
  | Favorited Username

type Input
  = Record
      ( WithCommon
          ( page :: SubPage
          )
      )

extract :: SubPage -> Username
extract (Authored u) = u

extract (Favorited u) = u

dispatch :: forall a. a -> a -> SubPage -> a
dispatch a _ (Authored _) = a

dispatch _ a (Favorited _) = a

type Slot
  = H.Slot Query Output

type State
  = Record
      ( WithCommon
          ( profile :: LoadState Profile
          , page :: SubPage
          , articles :: LoadState ArticleList
          )
      )

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
initialState { page, currentUser, urls } =
  { profile: Loading
  , page
  , articles: Loading
  , currentUser
  , urls
  }

render :: forall m. State -> HH.ComponentHTML Action ChildSlots m
render state =
  let
    showFavorites = dispatch false true state.page

    articleClass = if showFavorites then [ BS.navLink ] else [ BS.navLink, BS.active ]

    favoriteClass = if showFavorites then [ BS.navLink, BS.active ] else [ BS.navLink ]
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
                                    ( \u ->
                                        if u.username == profile.username then
                                          const $ HH.div_ []
                                        else
                                          Utils.followButton (preventDefault <<< FollowButtonClicked)
                                    )
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
  showArticles articles = ArticlePreview.renderArticleList articles $ preventDefault <<< FavoritedButtonClicked

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
    handleAction (Receive { page: state.page, currentUser: state.currentUser, urls: state.urls })
  Receive { page, currentUser, urls } -> do
    let
      token = currentUser <#> _.token
    parSequence_ [ loadProfile urls page token, loadArticles urls page token ]
    H.modify_ (_ { page = page, currentUser = currentUser, urls = urls })
  FavoritedButtonClicked article -> do
    { currentUser, urls } <- H.get
    let
      token = currentUser <#> _.token
    token
      # maybe (H.raise (Redirect loginUrl)) \tok ->
          Utils.favorite urls article tok (\art -> _ { articles = art }) _.articles
  FollowButtonClicked profile -> do
    { currentUser, urls } <- H.get
    currentUser
      # maybe (H.raise (Redirect loginUrl)) \user ->
          Utils.follow urls profile user.token (\prof -> _ { profile = prof })
  PreventDefault event action -> Utils.preventDefault event action handleAction
  where
  setArticles c u v s = s { articles = v, page = c u }

  loadProfile urls username token = load (API.getProfile urls (extract username) token) (\v -> _ { profile = v })

  loadArticles urls (Authored username) token = load (API.getUserArticles urls username token) $ setArticles Authored username

  loadArticles urls (Favorited username) token = load (API.getFavorites urls username token) $ setArticles Favorited username
