module Pages.Home where

import Prelude
import API as API
import Classes as C
import Control.Parallel (parSequence_)
import Data.Article (Article)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.GlobalState as GlobalState
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
import Templates.ArticlePreview as ArticlePreview
import Utils as Utils
import Web.Event.Event (Event)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

type Query
  = Const Void

type Output
  = Void

type Slot
  = H.Slot Query Output

type Input
  = GlobalState.State

data Tab
  = GlobalFeed
  | TagFeed Tag
  | PersonalFeed User

type State
  = { tags :: LoadState (Array Tag)
    , articles :: LoadState (Array Article)
    , selected :: Tab
    , currentUser :: Maybe User
    }

data Action
  = Init
  | Receive (Maybe User)
  | TabSelected Tab
  | PreventDefault Event (Maybe Action)
  | Favorited Article

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
              , receive = Just <<< Receive <<< _.currentUser
              }
    }

initialState :: Input -> State
initialState { currentUser } =
  { articles: Loading
  , tags: Loading
  , selected: maybe GlobalFeed PersonalFeed currentUser
  , currentUser
  }

render :: forall m. State -> HH.ComponentHTML Action ChildSlots m
render state =
  let
    articles = case state.articles of
      Loading -> [ HH.text "Loading" ]
      Loaded as -> map (ArticlePreview.render <*> preventDefault <<< Favorited) as
      LoadError error -> [ Utils.errorDisplay error ]

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
  forall o m.
  MonadAff m =>
  Action ->
  H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  Init -> do
    state <- H.gets _.currentUser
    handleAction (Receive state)
  Receive user ->
    let
      loadArts = maybe (loadArticles Nothing) loadPersonal user
    in
      do
        parSequence_ [ loadArts, loadTags ]
  TabSelected tab -> do
    state <- H.get
    let
      currentTab = state.selected
    let
      user = state.currentUser
    case tab of
      TagFeed tag -> case currentTab of
        TagFeed currentTag ->
          if tag /= currentTag then
            loadTagged tag (user <#> _.token)
          else
            pure unit
        _ -> loadTagged tag (user <#> _.token)
      GlobalFeed -> case currentTab of
        GlobalFeed -> pure unit
        _ -> do
          loadGlobal (user <#> _.token)
      PersonalFeed u -> case currentTab of
        PersonalFeed _ -> pure unit
        _ -> loadPersonal u
  Favorited article -> do
    user <- H.gets _.currentUser
    let
      token = user <#> _.token
    token # maybe (pure unit) \tok -> 
      Utils.favorite article tok updateArticles _.articles

  PreventDefault event action -> do
    Utils.preventDefault event action handleAction
  where
  updateArticles v = _ { articles = v }
  loadArticles token = load (API.getArticles token) updateArticles

  loadGlobal token = load (API.getArticles token) (\v -> _ { articles = v, selected = GlobalFeed })

  loadTagged tag token = load (API.getTaggedArticles tag token) (\v -> _ { articles = v, selected = TagFeed tag })

  loadTags = load API.getTags (\v -> _ { tags = v })

  loadPersonal user =
    load (API.getFeed user.token)
      ( \v ->
          _
            { articles = v
            , selected = PersonalFeed user
            , currentUser = Just user
            }
      )
