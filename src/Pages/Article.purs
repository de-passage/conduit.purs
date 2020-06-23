module Pages.Article where

import Prelude
import API as API
import Classes as C
import Control.Comonad (extract)
import Control.Parallel (parSequence_)
import Data.Array (cons)
import Data.Article (Article, Slug)
import Data.ButtonStatus (ButtonStatus(..), disabled)
import Data.Comment (Comment, CommentId)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.GlobalState (WithCommon)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Data.User (User, Profile, fromImage)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as BS
import LoadState (LoadState(..), load)
import Marked as Marked
import Router (editArticleUrl, homeUrl, loginUrl, profileUrl)
import Utils as Utils
import Web.Event.Internal.Types (Event)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

type Query
  = Const Void

data Output
  = Redirect String

type Input
  = Record (WithCommon ( slug :: Slug ))

type Slot
  = H.Slot Query Output

type State
  = Record
      ( WithCommon
          ( article :: LoadState Article
          , comments :: LoadState (Array Comment)
          , slug :: Slug
          , comment :: String
          , commentButtonStatus :: ButtonStatus
          )
      )

data Action
  = Init
  | Receive Input
  | FavoriteButtonClicked Article
  | FollowButtonClicked Profile
  | PreventDefault Event (Maybe Action)
  | DeleteArticle Article
  | EditArticle Article
  | ChangeComment String
  | PublishComment User Article
  | DeleteComment CommentId Article User

type ChildSlots
  = ( marked :: Marked.Slot Unit )

_marked = SProxy :: SProxy "marked"

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
initialState { slug, currentUser, urls } =
  { urls
  , article: Loading
  , comments: Loading
  , slug
  , currentUser
  , comment: ""
  , commentButtonStatus: Active
  }

render :: forall m. MonadEffect m => State -> HH.ComponentHTML Action ChildSlots m
render state = case state.article of
  Loading -> HH.div_ [ HH.text "Loading article" ]
  LoadError error -> HH.div [ HP.class_ C.articlePage ] [ HH.div [ HP.class_ BS.alertDanger ] [ Utils.errorDisplay error ] ]
  Loaded article ->
    HH.div [ HP.class_ C.articlePage ]
      [ HH.div [ HP.class_ C.banner ]
          [ HH.div [ HP.class_ BS.container ]
              [ HH.h1_ [ HH.text article.title ]
              , articleMeta state.currentUser article
              ]
          ]
      , HH.div [ HP.classes [ BS.container, C.page ] ]
          [ HH.div [ HP.classes [ BS.row, C.articleContent ] ]
              [ HH.div [ HP.class_ BS.colMd12 ]
                  [ HH.slot _marked unit Marked.component { text: article.body } absurd
                  ]
              ]
          , HH.hr_
          , HH.div [ HP.class_ C.articleActions ]
              [ articleMeta state.currentUser article
              ]
          , HH.div [ HP.class_ BS.row ]
              [ HH.div [ HP.classes [ C.colXs12, BS.colMd8, BS.offsetMd2 ] ]
                  ( comments state.comment article state.currentUser state.commentButtonStatus state.comments
                  )
              ]
          ]
      ]

handleAction ∷
  forall m.
  MonadAff m =>
  Action ->
  H.HalogenM State Action ChildSlots Output m Unit
handleAction = case _ of
  Init -> do
    state <- H.get
    handleAction (Receive { slug: state.slug, currentUser: state.currentUser, urls: state.urls })
  Receive { slug, currentUser, urls } -> do
    H.modify_ _ { slug = slug, currentUser = currentUser, urls = urls }
    let
      token = currentUser <#> _.token
    parSequence_ [ loadArticle urls slug token, loadComments urls slug token ]
  FavoriteButtonClicked article -> do
    state <- H.get
    state.currentUser
      # maybe (H.raise (Redirect loginUrl)) \u ->
          Utils.favorite
            state.urls
            article
            u.token
            (map extract >>> setArticle)
            (_.article >>> map Identity)
  FollowButtonClicked profile -> do
    state <- H.get
    state.currentUser
      # maybe (H.raise (Redirect loginUrl)) \{ token } ->
          Utils.follow state.urls profile token \v s -> case v of
            Loaded a -> s { article = s.article <#> (_ { author = a }) }
            _ -> s
  PreventDefault event action -> do
    Utils.preventDefault event action handleAction
  EditArticle article -> H.raise $ Redirect $ editArticleUrl article.slug
  DeleteArticle { slug } -> do
    { currentUser, urls } <- H.get
    currentUser
      # maybe (H.raise $ Redirect loginUrl) \{ token } -> do
          _ <- H.liftAff $ API.request $ API.articleDeletion urls slug token
          H.raise $ Redirect homeUrl
  ChangeComment comment -> H.modify_ _ { comment = comment }
  PublishComment { token } { slug } -> do
    { comment, urls } <- H.get
    if comment /= "" then do
      H.modify_ _ { commentButtonStatus = Inactive }
      req <- H.liftAff $ API.request $ API.commentCreation urls slug { comment: { body: comment } } token
      case req of
        Left err -> H.modify_ _ { commentButtonStatus = Active }
        Right _ -> do
          H.modify_ _ { comment = "", commentButtonStatus = Active }
          loadComments urls slug $ Just token
    else
      pure unit
  DeleteComment id { slug } { token } -> do
    urls <- H.gets _.urls
    void $ H.liftAff $ API.request $ API.commentDeletion urls slug id token
    loadComments urls slug $ Just token
  where
  loadArticle url slug token = load (API.getArticle url slug token) setArticle

  loadComments url slug token = load (API.getComments url slug token) (\v -> _ { comments = v })

  setArticle v = _ { article = v }

articleMeta :: forall w. Maybe User -> Article -> HH.HTML w Action
articleMeta user article =
  HH.div [ HP.class_ C.articleMeta ]
    ( [ HH.a [ HP.href (profileUrl article.author.username) ]
          [ HH.img [ HP.src $ fromImage article.author.image ] ]
      , HH.div [ HP.class_ C.info ]
          [ HH.a [ HP.href (profileUrl article.author.username), HP.class_ C.author ] [ HH.text $ unwrap article.author.username ]
          , HH.span [ HP.class_ C.date ] [ HH.text $ Utils.hackyFormatDate article.createdAt ]
          ]
      ]
        <> buttons
    )
  where
  buttons :: Array (HH.HTML w Action)
  buttons = case user of
    Nothing -> normalBtns
    Just u ->
      if u.username == article.author.username then
        selfBtns
      else
        normalBtns

  normalBtns =
    [ followButton article.author
    , HH.text "  "
    , favoriteButton
    ]

  selfBtns =
    [ editButton
    , HH.text "  "
    , deleteButton
    ]

  favoriteButton :: HH.HTML w Action
  favoriteButton =
    HH.button
      [ HP.classes [ BS.btn, BS.btnSm, if article.favorited then BS.btnOutlinePrimary else BS.btnPrimary ]
      , HE.onClick $ preventDefault (FavoriteButtonClicked article)
      ]
      [ HH.i [ HP.class_ C.ionHeart ] []
      , HH.text (if article.favorited then " Unfavorite Post " else " Favorite Post ")
      , HH.span [ HP.class_ C.counter ] [ HH.text $ "(" <> show article.favoritesCount <> ") " ]
      ]

  editButton :: HH.HTML w Action
  editButton =
    HH.button
      [ HP.classes [ BS.btn, BS.btnSm, BS.btnOutlineSecondary ]
      , HE.onClick $ preventDefault (EditArticle article)
      ]
      [ HH.i [ HP.class_ C.ionEdit ] []
      , HH.text " Edit Article"
      ]

  deleteButton :: HH.HTML w Action
  deleteButton =
    HH.button
      [ HP.classes [ BS.btn, BS.btnSm, BS.btnOutlineDanger ]
      , HE.onClick $ preventDefault (DeleteArticle article)
      ]
      [ HH.i [ HP.class_ C.ionTrashA ] []
      , HH.text " Delete Article"
      ]

  followButton :: Profile -> HH.HTML w Action
  followButton = Utils.followButtonC [ BS.btnSm ] (preventDefault <<< FollowButtonClicked)

preventDefault :: Action -> MouseEvent -> Maybe Action
preventDefault action event = Just $ PreventDefault (toEvent event) $ Just action

comments :: forall w. String -> Article -> Maybe User -> ButtonStatus -> LoadState (Array Comment) -> Array (HH.HTML w Action)
comments currentComment article user btn = case _ of
  Loading -> [ HH.div_ [ HH.text "Loading comments" ] ]
  LoadError err -> [ HH.div [ HP.class_ BS.alertDanger ] [ Utils.errorDisplay err ] ]
  Loaded cs ->
    ( HH.form [ HP.classes [ BS.card, C.commentForm ] ]
        commentEdition
    )
      `cons`
        map mkComment cs
  where
  mkComment :: Comment -> HH.HTML w Action
  mkComment comment =
    HH.div [ HP.class_ BS.card ]
      [ HH.div [ HP.class_ C.cardBlock ]
          [ HH.p [ HP.class_ BS.cardText ] [ HH.text comment.body ]
          ]
      , HH.div [ HP.class_ BS.cardFooter ]
          [ HH.a [ HP.href (profileUrl comment.author.username), HP.class_ C.commentAuthor ]
              [ HH.img [ HP.src $ fromImage comment.author.image, HP.class_ C.commentAuthorImg ]
              ]
          , HH.text " "
          , HH.a [ HP.href (profileUrl comment.author.username), HP.class_ C.commentAuthor ]
              [ HH.text $ unwrap comment.author.username ]
          , HH.span [ HP.class_ C.datePosted ] [ HH.text $ Utils.hackyFormatDate comment.createdAt ]
          , HH.span [ HP.class_ C.modOptions ] (options comment.id)
          ]
      ]

  options :: CommentId -> Array (HH.HTML w Action)
  options commentId =
    user
      # maybe [] \u ->
          [ HH.a [ HP.href "", HE.onClick $ preventDefault $ DeleteComment commentId article u ]
              [ HH.i [ HP.class_ C.ionTrashA ] [] ]
          ]

  commentEdition :: Array (HH.HTML w Action)
  commentEdition =
    user
      # maybe [ HH.div_ [ HH.a [ HP.href loginUrl ] [ HH.text "Log in to comment" ] ] ] \u ->
          [ HH.div [ HP.class_ C.cardBlock ]
              [ HH.textarea
                  [ HP.class_ BS.formControl
                  , HP.rows 3
                  , HP.placeholder "Write a comment..."
                  , HE.onValueChange $ Just <<< ChangeComment
                  , HP.value currentComment
                  ]
              ]
          , HH.div [ HP.class_ BS.cardFooter ]
              [ HH.img [ HP.src $ fromImage u.image, HP.class_ C.commentAuthorImg ]
              , HH.button
                  [ HP.classes [ BS.btn, BS.btnSm, BS.btnPrimary ]
                  , HE.onClick $ preventDefault $ PublishComment u article
                  , disabled btn
                  ]
                  [ HH.text "Post Comment"
                  ]
              ]
          ]
