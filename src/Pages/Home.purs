module Pages.Home where

import Prelude
import API as API
import Classes as C
import Control.Parallel (parSequence_)
import Data.Article as A
import Data.Const (Const)
import Data.DefaultPreventable (class DefaultPreventable, defaultPreventDefaults)
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
  = Record (WithCommon ( perPage :: A.PerPage ))

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
  | RequestNewPage A.PageNumber A.Offset

instance defaultsPreventableAction :: DefaultPreventable Action where
  action = PreventDefault
  preventDefaults = defaultPreventDefaults

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
  , pageNumber: A.emptyPageNumber
  }

render :: forall m. State -> HH.ComponentHTML Action ChildSlots m
render state =
  let
    articles =
      ArticlePreview.renderArticleList
        (A.mkDisplaySettings state.pageNumber state.perPage)
        (\a b -> preventDefault $ RequestNewPage a b)
        state.articles
        $ preventDefault
        <<< Favorited

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
                      [ HH.ul [ HP.classes [ BS.nav, BS.navPills, C.outlineActive ] ] tabs
                      , articles
                      ]
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
  Receive { currentUser, urls, perPage } -> do
    pageNumber <- H.gets _.pageNumber
    let
      loadArts =
        maybe
          (loadArticles urls A.noOffset pageNumber perPage Nothing)
          (loadPersonal urls A.noOffset pageNumber perPage)
          currentUser
    parSequence_ [ loadArts, loadTags urls ]
  TabSelected tab -> do
    { selected, currentUser, urls, perPage } <- H.get
    let
      token = currentUser <#> _.token
    case tab of
      TagFeed tag -> case selected of
        TagFeed currentTag ->
          if tag /= currentTag then
            loadTagged urls A.noOffset A.emptyPageNumber perPage tag token
          else
            pure unit
        _ -> loadTagged urls A.noOffset A.emptyPageNumber perPage tag token
      GlobalFeed -> case selected of
        GlobalFeed -> pure unit
        _ -> do
          loadGlobal urls A.noOffset A.emptyPageNumber perPage token
      PersonalFeed u -> case selected of
        PersonalFeed _ -> pure unit
        _ -> loadPersonal urls A.noOffset A.emptyPageNumber perPage u
  Favorited article -> do
    { currentUser, urls } <- H.get
    let
      token = currentUser <#> _.token
    token
      # maybe (H.raise (Redirect loginUrl)) \tok ->
          Utils.favorite urls article tok updateArticles _.articles
  PreventDefault event action -> do
    Utils.preventDefault event action handleAction
  RequestNewPage pn offset -> do
    { selected, urls, currentUser, perPage } <- H.get
    let
      token = currentUser <#> _.token
    case selected of
      TagFeed tag -> loadTagged urls offset pn perPage tag token
      GlobalFeed -> loadGlobal urls offset pn perPage token
      PersonalFeed u -> loadPersonal urls offset pn perPage u
  where
  updateArticles :: LoadState A.ArticleList -> State -> State
  updateArticles v = _ { articles = v }

  loadArticles urls offset pn pp token =
    load (API.getArticles urls offset pp token)
      (\v -> _ { articles = v, pageNumber = pn })

  loadGlobal urls offset pn pp token =
    load (API.getArticles urls offset pp token)
      (\v -> _ { articles = v, selected = GlobalFeed, pageNumber = pn })

  loadTagged urls offset pn pp tag token =
    load (API.getTaggedArticles urls tag offset pp token)
      (\v -> _ { articles = v, selected = TagFeed tag, pageNumber = pn })

  loadTags urls = load (API.getTags urls) (\v -> _ { tags = v })

  loadPersonal urls off pn pp user =
    load (API.getFeed urls pp off user.token)
      ( \v ->
          _
            { articles = v
            , selected = PersonalFeed user
            , currentUser = Just user
            , pageNumber = pn
            }
      )
