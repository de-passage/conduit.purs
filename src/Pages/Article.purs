module Pages.Article where

import Prelude

import API as API
import Classes as C
import Control.Parallel (parSequence_)
import Data.Article (Article, Slug)
import Data.Comment (Comment)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.User (fromImage)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as BS
import LoadState (LoadState(..), load)
import Router (profileUrl)
import Utils as Utils

type Query
  = Const Void

type Output
  = Void

type Input
  = Slug

type Slot
  = H.Slot Query Output

type State
  = { article :: LoadState Article
    , comments :: LoadState (Array Comment)
    , slug :: Slug
    }

data Action
  = Init
  | Receive Slug

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
initialState slug = { article: Loading, comments: Loading, slug }

render :: forall m. State -> HH.ComponentHTML Action ChildSlots m
render state = case state.article of
  Loading -> HH.div_ [ HH.text "Loading article" ]
  LoadError error -> HH.div [ HP.class_ C.articlePage ] [ HH.div [ HP.class_ BS.alertDanger ] [ Utils.errorDisplay error ] ]
  Loaded article ->
    HH.div [ HP.class_ C.articlePage ]
      [ HH.div [ HP.class_ C.banner ]
          [ HH.div [ HP.class_ BS.container ]
              [ HH.h1_ [ HH.text article.title ]
              , articleMeta article
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
              [ articleMeta article
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
    handleAction (Receive state.slug)
  Receive slug -> do
    parSequence_ [ loadArticle slug, loadComments slug ]
  where
  loadArticle slug = load (API.getArticle slug Nothing) (\v -> _ { article = v })

  loadComments slug = load (API.getComments slug Nothing) (\v -> _ { comments = v })

articleMeta :: forall w i. Article -> HH.HTML w i
articleMeta article =
  HH.div [ HP.class_ C.articleMeta ]
    [ HH.a [ HP.href (profileUrl article.author.username) ]
        [ HH.img [ HP.src $ fromImage article.author.image ] ]
    , HH.div [ HP.class_ C.info ]
        [ HH.a [ HP.href (profileUrl article.author.username), HP.class_ C.author ] [ HH.text $ unwrap article.author.username ]
        , HH.span [ HP.class_ C.date ] [ HH.text article.createdAt ]
        ]
    , HH.button [ HP.classes [ BS.btn, BS.btnSm, BS.btnOutlineSecondary ] ]
        [ HH.i [ HP.class_ C.ionPlusRound ] []
        , HH.text $ " Follow " <> unwrap article.author.username <> " "
        -- , HH.span [ HP.class_ C.counter ] [ HH.text $ "(" <> show article.author.followers <> ")" ]
        ]
    , HH.text "  "
    , HH.button [ HP.classes [ BS.btn, BS.btnSm, BS.btnOutlinePrimary ] ]
        [ HH.i [ HP.class_ C.ionHeart ] []
        , HH.text " Favorite Post "
        , HH.span [ HP.class_ C.counter ] [ HH.text $ "(" <> show article.favoritesCount <> ") " ]
        ]
    ]

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
