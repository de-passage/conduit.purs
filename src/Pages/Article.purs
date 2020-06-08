module Pages.Article where

import Prelude
import API as API
import Classes as C
import Control.Comonad (extract)
import Control.Parallel (parSequence_)
import Data.Article (Article, Slug)
import Data.Comment (Comment)
import Data.Const (Const)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.User (User, Profile, fromImage)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as BS
import LoadState (LoadState(..), load)
import Router (profileUrl)
import Utils as Utils
import Web.Event.Internal.Types (Event)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

type Query
  = Const Void

type Output
  = Void

type Input
  = { slug :: Slug, currentUser :: Maybe User }

type Slot
  = H.Slot Query Output

type State
  = { article :: LoadState Article
    , comments :: LoadState (Array Comment)
    , slug :: Slug
    , currentUser :: Maybe User
    }

data Action
  = Init
  | Receive Input
  | FavoriteButtonClicked Article
  | FollowButtonClicked Profile
  | PreventDefault Event (Maybe Action)

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
initialState { slug, currentUser } = { article: Loading, comments: Loading, slug, currentUser }

render :: forall m. State -> HH.ComponentHTML Action ChildSlots m
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
                  [ HH.text article.body
                  ]
              ]
          , HH.hr_
          , HH.div [ HP.class_ C.articleActions ]
              [ articleMeta state.currentUser article
              ]
          , HH.div [ HP.class_ BS.row ]
              [ HH.div [ HP.classes [ C.colXs12, BS.colMd8, BS.offsetMd2 ] ]
                  [ HH.form [ HP.classes [ BS.card, C.commentForm ] ]
                      ( [ HH.div [ HP.class_ C.cardBlock ]
                            [ HH.textarea [ HP.class_ BS.formControl, HP.rows 3, HP.placeholder "Write a comment..." ]
                            ]
                        , HH.div [ HP.class_ BS.cardFooter ]
                            [ HH.img [ HP.src "http://i.imgur.com/Qr71crq.jpg", HP.class_ C.commentAuthorImg ]
                            , HH.button [ HP.classes [ BS.btn, BS.btnSm, BS.btnPrimary ] ]
                                [ HH.text "Post Comment"
                                ]
                            ]
                        ]
                          <> comments state.comments
                      )
                  ]
              ]
          ]
      ]

handleAction ∷
  forall o m.
  MonadAff m =>
  Action ->
  H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  Init -> do
    state <- H.get
    handleAction (Receive { slug: state.slug, currentUser: state.currentUser })
  Receive { slug, currentUser } -> do
    H.modify_ _ { slug = slug, currentUser = currentUser }
    let
      token = currentUser <#> _.token
    parSequence_ [ loadArticle slug token, loadComments slug token ]
  FavoriteButtonClicked article -> do
    state <- H.get
    state.currentUser
      # maybe (pure unit) \u ->
          Utils.favorite
            article
            u.token
            (map extract >>> setArticle)
            (_.article >>> map Identity)
  FollowButtonClicked profile -> do
    state <- H.get
    state.currentUser
      # maybe (pure unit) \u ->
          Utils.follow profile u.token \v s -> case v of
            Loaded a -> s { article = s.article <#> (_ { author = a }) }
            _ -> s
  PreventDefault event action -> do
    Utils.preventDefault event action handleAction
  where
  loadArticle slug token = load (API.getArticle slug token) setArticle

  loadComments slug token = load (API.getComments slug token) (\v -> _ { comments = v })

  setArticle v = _ { article = v }

articleMeta :: forall w. Maybe User -> Article -> HH.HTML w Action
articleMeta currentUser article =
  HH.div [ HP.class_ C.articleMeta ]
    [ HH.a [ HP.href (profileUrl article.author.username) ]
        [ HH.img [ HP.src $ fromImage article.author.image ] ]
    , HH.div [ HP.class_ C.info ]
        [ HH.a [ HP.href (profileUrl article.author.username), HP.class_ C.author ] [ HH.text $ unwrap article.author.username ]
        , HH.span [ HP.class_ C.date ] [ HH.text article.createdAt ]
        ]
    , ( currentUser
          # maybe (const $ HH.span_ [])
              ( \u ->
                  if u.username == article.author.username then
                    const $ HH.span_ []
                  else
                    Utils.followButtonC [ BS.btnSm ] (preventDefault <<< FollowButtonClicked)
              )
      )
        $ article.author
    , HH.text "  "
    , HH.button
        [ HP.classes [ BS.btn, BS.btnSm, if article.favorited then BS.btnOutlinePrimary else BS.btnPrimary ]
        , HE.onClick $ preventDefault (FavoriteButtonClicked article)
        ]
        [ HH.i [ HP.class_ C.ionHeart ] []
        , HH.text (if article.favorited then " Unfavorite Post " else " Favorite Post ")
        , HH.span [ HP.class_ C.counter ] [ HH.text $ "(" <> show article.favoritesCount <> ") " ]
        ]
    ]

preventDefault :: Action -> MouseEvent -> Maybe Action
preventDefault action event = Just $ PreventDefault (toEvent event) $ Just action

comments :: forall w i. LoadState (Array Comment) -> Array (HH.HTML w i)
comments = case _ of
  Loading -> [ HH.div_ [ HH.text "Loading comments" ] ]
  LoadError err -> [ HH.div [ HP.class_ BS.alertDanger ] [ Utils.errorDisplay err ] ]
  Loaded cs -> map mkComment cs
  where
  mkComment :: Comment -> HH.HTML w i
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
          , HH.span [ HP.class_ C.datePosted ] [ HH.text $ comment.createdAt ]
          , HH.span [ HP.class_ C.modOptions ]
              [ HH.i [ HP.class_ C.ionEdit ] []
              , HH.i [ HP.class_ C.ionTrashA ] []
              ]
          ]
      ]
