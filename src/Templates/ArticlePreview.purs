module Templates.ArticlePreview where

import Prelude
import Classes as C
import Data.Array (snoc)
import Data.Article (Article, ArticleCount, ArticleDisplaySettings, ArticleList, Distance(..), Offset, Page(..), PageNumber, _articlesCount, _pageNumber, firstPage, foldPages, fromArticles, fromPageNumber, isFirst, isLast, lastPage, nextPage, pageCount, previousPage, toOffset)
import Data.DefaultPreventable (class DefaultPreventable, preventDefaults)
import Data.Lens (view, (^.))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.User (fromImage)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as BS
import LoadState (LoadState(..))
import Router (profileUrl, showArticleUrl)
import Utils as Utils
import Web.UIEvent.MouseEvent (MouseEvent)

renderArticle :: forall w i. Article -> (MouseEvent -> Maybe i) -> HH.HTML w i
renderArticle article favorite =
  let
    username = unwrap article.author.username

    userUrl = profileUrl article.author.username

    btnStyle = if article.favorited then BS.btnPrimary else BS.btnOutlinePrimary
  in
    HH.div [ HP.class_ C.articlePreview ]
      [ HH.div [ HP.class_ C.articleMeta ]
          [ HH.a [ HP.href userUrl ] [ HH.img [ HP.src $ fromImage article.author.image ] ]
          , HH.div [ HP.class_ C.info ]
              [ HH.a [ HP.href userUrl, HP.class_ C.author ] [ HH.text username ]
              , HH.span [ HP.class_ C.date ] [ HH.text $ Utils.hackyFormatDate article.createdAt ]
              ]
          , HH.button [ HP.classes [ BS.btn, btnStyle, BS.btnSm, C.pullXsRight ], HE.onClick favorite ]
              [ HH.i [ HP.class_ C.ionHeart ] [], HH.text (" " <> show article.favoritesCount)
              ]
          , HH.a [ HP.href (showArticleUrl article.slug), HP.class_ C.previewLink ]
              [ HH.h1_ [ HH.text article.title ]
              , HH.p_ [ HH.text article.description ]
              , HH.span_ [ HH.text "Read more..." ]
              , HH.ul [ HP.class_ C.tagList ] (map mkTag article.tagList)
              ]
          ]
      ]
  where
  mkTag tag = HH.li [ HP.classes [ C.tagDefault, C.tagPill, C.tagOutline ] ] [ HH.text $ unwrap tag ]

renderArticleList ::
  forall w i.
  DefaultPreventable i =>
  (ArticleCount -> ArticleDisplaySettings) ->
  (PageNumber -> Offset -> MouseEvent -> Maybe i) ->
  LoadState ArticleList ->
  (Article -> MouseEvent -> Maybe i) ->
  HH.HTML w i
renderArticleList mkSettings loadArts list favorite = case list of
  Loading -> HH.div_ [ HH.text "Loading" ]
  Loaded as -> HH.div_ ((fromArticles (renderArticle <*> favorite) as) `snoc` pagination (mkSettings $ view _articlesCount as))
  LoadError error -> Utils.errorDisplay error
  where
  pagination :: ArticleDisplaySettings -> HH.HTML w i
  pagination settings =
    if pageCount settings > 1 then
      HH.nav_
        [ HH.ul [ HP.class_ BS.pagination ]
            (first settings <> listPages settings loadArts <> last settings)
        ]
    else
      HH.div_ []

  item :: ArticleDisplaySettings -> String -> (PageNumber -> Offset -> MouseEvent -> Maybe i) -> Boolean -> HH.HTML w i
  item settings text action isActive =
    HH.li [ HP.class_ BS.pageItem ]
      [ HH.a
          ( if isActive then
              [ HP.href ""
              , HE.onClick
                  (action (settings ^. _pageNumber) (toOffset settings))
              , HP.classes [ BS.pageLink ]
              ]
            else
              [ HE.onClick $ preventDefaults (Nothing :: Maybe i)
              , HP.classes [ BS.pageLink ]
              , HP.tabIndex (-1)
              ]
          )
          [ HH.text text
          ]
      ]

  first :: ArticleDisplaySettings -> Array (HH.HTML w i)
  first settings =
    [ item (firstPage settings) "First" loadArts (not $ isFirst settings)
    , item (previousPage settings) "Previous" loadArts (not $ isFirst settings)
    ]

  last :: ArticleDisplaySettings -> Array (HH.HTML w i)
  last settings =
    [ item (nextPage settings) "Next" loadArts (not $ isLast settings)
    , item (lastPage settings) "Last" loadArts (not $ isLast settings)
    ]

  listPages :: ArticleDisplaySettings -> (PageNumber -> Offset -> MouseEvent -> Maybe i) -> Array (HH.HTML w i)
  listPages settings action =
    foldPages
      settings
      { arr: [], fillerInserted: false }
      (folder action)
      # _.arr

  folder ::
    (PageNumber -> Offset -> MouseEvent -> Maybe i) ->
    { arr :: Array (HH.HTML w i), fillerInserted :: Boolean } ->
    Page ->
    { arr :: Array (HH.HTML w i), fillerInserted :: Boolean }
  folder action { arr, fillerInserted } = case _ of
    CurrentPage pn ->
      let
        el =
          HH.li [ HP.class_ BS.pageItem ]
            [ HH.a
                [ HE.onClick (preventDefaults (Nothing :: Maybe i))
                , HP.classes [ BS.pageLink, BS.active, BS.btnPrimary ]
                , HP.tabIndex (-1)
                ]
                [ HH.text $ show $ fromPageNumber pn + 1
                ]
            ]
      in
        { arr: arr `snoc` el, fillerInserted: false }
    OtherPage pn offset (Distance fromCurrent) (Distance fromEdge) ->
      if fromCurrent > 1 && fromEdge > 1 then
        if fillerInserted then
          { arr, fillerInserted }
        else
          let
            filler =
              HH.li [ HP.class_ BS.pageItem ]
                [ HH.a
                    [ HE.onClick (preventDefaults (Nothing :: Maybe i))
                    , HP.classes [ BS.disabled, BS.pageLink ]
                    , HP.tabIndex (-1)
                    ]
                    [ HH.text "..." ]
                ]
          in
            { arr: arr `snoc` filler, fillerInserted: true }
      else
        let
          el =
            HH.li [ HP.class_ BS.pageItem ]
              [ HH.a
                  [ HP.href ""
                  , HE.onClick $ action pn offset
                  , HP.class_ BS.pageLink
                  ]
                  [ HH.text $ show $ fromPageNumber pn + 1
                  ]
              ]
        in
          { arr: arr `snoc` el, fillerInserted: false }
