module Pages.Home where

import Prelude
import API as API
import Classes as C
import Control.Parallel (parSequence_)
import Data.Article as A
import Data.Const (Const)
import Data.GlobalState (WithCommon, Paginated)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Tag (Tag)
import Data.User (User)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as BS
import LoadState (LoadState(..), load)
import Router (loginUrl)
import Templates.ArticlePreview as ArticlePreview
import Utils as Utils
import Web.Event.Event (Event)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

type Query
  = Const Void

data Output
  = Redirect String

type Slot
  = H.Slot Query Output

type Input
  = Record (Paginated (WithCommon ()))

data Tab
  = GlobalFeed
  | TagFeed Tag
  | PersonalFeed User

type State
  = Record
      ( Paginated
          ( WithCommon
              ( tags :: LoadState (Array Tag)
              , articles :: LoadState A.ArticleList
              , selected :: Tab
              )
          )
      )

data Action
  = Init
  | Receive Input
  | TabSelected Tab
  | PreventDefault Event (Maybe Action)
  | Favorited A.Article

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
              , initialize = Just Init
              , receive = Just <<< Receive
              }
    }

initialState :: Input -> State
initialState { currentUser, urls, perPage } =
  { articles: Loading
  , tags: Loading
  , selected: maybe GlobalFeed PersonalFeed currentUser
  , currentUser
  , urls
  , perPage
  }

render :: forall m. State -> HH.ComponentHTML Action ChildSlots m
render state =
  let
    articles = ArticlePreview.renderArticleList (A.firstPage state.perPage) state.articles $ preventDefault <<< Favorited

    tagList = case state.tags of
      Loading -> HH.div_ []
      Loaded tags ->
        HH.div [ HP.class_ C.sidebar ]
          [ HH.p_ [ HH.text "Popular Tags" ]
          , HH.div [ HP.class_ C.tagList ]
              ( map tagLink tags
              )
          ]
      LoadError error -> Utils.errorDisplay error

    feedTab text tab selected =
      let
        props =
          if selected then
            [ HP.classes [ BS.navLink, BS.active ]
            , HP.href ""
            , HE.onClick (\e -> Just $ PreventDefault (toEvent e) Nothing)
            ]
          else
            [ HP.classes [ BS.navLink ]
            , HP.href ""
            , HE.onClick (selectTab tab)
            ]
      in
        HH.li [ HP.class_ BS.navItem ]
          [ HH.a props
              [ HH.text text
              ]
          ]

    globalFeed = feedTab "Global Feed" GlobalFeed

    personalFeed user = feedTab "Personal Feed" (PersonalFeed user)

    tagFeed tag = feedTab ("#" <> unwrap tag) (TagFeed tag)

    tabs = case state.currentUser of
      Nothing -> case state.selected of
        TagFeed tag -> [ globalFeed false, tagFeed tag true ]
        _ -> [ globalFeed true ]
      Just user -> case state.selected of
        TagFeed tag -> [ personalFeed user false, globalFeed false, tagFeed tag true ]
        GlobalFeed -> [ personalFeed user false, globalFeed true ]
        PersonalFeed _ -> [ personalFeed user true, globalFeed false ]
  in
    HH.div [ HP.class_ C.homePage ]
      [ HH.div [ HP.class_ C.banner ]
          [ HH.div [ HP.class_ BS.container ]
              [ HH.h1 [ HP.class_ C.logoFont ] [ HH.text "conduit" ]
              , HH.p_ [ HH.text "A place to share your knowledge." ]
              ]
          ]
      , HH.div [ HP.classes [ BS.container, C.page ] ]
          [ HH.div [ HP.class_ BS.row ]
              [ HH.div [ HP.class_ BS.colMd9 ]
                  [ HH.div [ HP.class_ C.feedToggle ]
                      ( [ HH.ul [ HP.classes [ BS.nav, BS.navPills, C.outlineActive ] ]
                            tabs
                        ]
                          <> articles
                      )
                  ]
              , HH.div [ HP.class_ BS.colMd3 ]
                  [ tagList
                  ]
              ]
          ]
      ]

tagLink :: forall w. Tag -> HH.HTML w Action
tagLink tag =
  HH.a
    [ HP.href ""
    , HP.classes [ C.tagPill, C.tagDefault ]
    , HE.onClick (selectTag tag)
    ]
    [ HH.text $ show tag ]

preventDefault :: Action -> MouseEvent -> Maybe Action
preventDefault action e = Just $ PreventDefault (toEvent e) $ Just $ action

selectTab :: Tab -> MouseEvent -> Maybe Action
selectTab tab = preventDefault $ TabSelected tab

selectTag :: Tag -> MouseEvent -> Maybe Action
selectTag tag = selectTab $ TagFeed tag

handleAction ∷
  forall m.
  MonadAff m =>
  Action ->
  H.HalogenM State Action ChildSlots Output m Unit
handleAction = case _ of
  Init -> do
    { currentUser, urls, perPage } <- H.get
    handleAction (Receive { currentUser, urls, perPage })
  Receive { currentUser, urls, perPage } ->
    let
      loadArts = maybe (loadArticles urls Nothing) (loadPersonal urls) currentUser
    in
      do
        parSequence_ [ loadArts, loadTags urls ]
  TabSelected tab -> do
    { selected, currentUser, urls } <- H.get
    case tab of
      TagFeed tag -> case selected of
        TagFeed currentTag ->
          if tag /= currentTag then
            loadTagged urls tag (currentUser <#> _.token)
          else
            pure unit
        _ -> loadTagged urls tag (currentUser <#> _.token)
      GlobalFeed -> case selected of
        GlobalFeed -> pure unit
        _ -> do
          loadGlobal urls (currentUser <#> _.token)
      PersonalFeed u -> case selected of
        PersonalFeed _ -> pure unit
        _ -> loadPersonal urls u
  Favorited article -> do
    { currentUser, urls } <- H.get
    let
      token = currentUser <#> _.token
    token
      # maybe (H.raise (Redirect loginUrl)) \tok ->
          Utils.favorite urls article tok updateArticles _.articles
  PreventDefault event action -> do
    Utils.preventDefault event action handleAction
  where
  updateArticles :: LoadState A.ArticleList -> State -> State
  updateArticles v = _ { articles = v }

  loadArticles urls token = load (API.getArticles urls token) updateArticles

  loadGlobal urls token = load (API.getArticles urls token) (\v -> _ { articles = v, selected = GlobalFeed })

  loadTagged urls tag token = load (API.getTaggedArticles urls tag token) (\v -> _ { articles = v, selected = TagFeed tag })

  loadTags urls = load (API.getTags urls) (\v -> _ { tags = v })

  loadPersonal urls user =
    load (API.getFeed urls user.token)
      ( \v ->
          _
            { articles = v
            , selected = PersonalFeed user
            , currentUser = Just user
            }
      )
