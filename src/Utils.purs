module Utils where

import Prelude

import API as API
import API.Response (Error, fromError)
import API.Response as R
import Data.Argonaut as A
import Data.Article (Article)
import Data.Either (Either(..))
import Data.Maybe (Maybe, maybe)
import Data.Token (Token)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console as C
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as BS
import LoadState (LoadState(..))
import Unsafe.Coerce as Unsafe
import Web.Event.Event (Event, preventDefault) as W

preventDefault ::
  forall s a c o m.
  MonadEffect m =>
  W.Event -> Maybe a -> (a -> H.HalogenM s a c o m Unit) -> H.HalogenM s a c o m Unit
preventDefault event action handle = do
  H.liftEffect $ W.preventDefault event
  maybe (pure unit) handle action

log ::
  forall s a c o m.
  MonadEffect m =>
  String -> H.HalogenM s a c o m Unit
log = H.liftEffect <<< C.log

unsafeLog ::
  forall s a c o m i.
  MonadEffect m =>
  i -> H.HalogenM s a c o m Unit
unsafeLog = H.liftEffect <<< C.log <<< Unsafe.unsafeCoerce

favorite ::
  forall a c o s m.
  MonadAff m =>
  Article ->
  Token ->
  (LoadState (Array Article) -> s -> s) ->
  (s -> LoadState (Array Article)) ->
  H.HalogenM s a c o m Unit
favorite article token update retrieve = do
  request <-
    H.liftAff
      $ API.request
          ( ( if article.favorited then
                API.unfavorite
              else
                API.favorite
            )
              article.slug
              token
          )
  case request of
    Left _ -> pure unit
    Right art -> H.modify_ $ replace art.article update retrieve

replace :: forall s. Article -> (LoadState (Array Article) -> s -> s) -> (s -> LoadState (Array Article)) -> s -> s
replace article update retrieve state = case retrieve state of
  Loaded arts ->
    update
      ( Loaded $ arts
          # map \a ->
              if a.slug == article.slug then article else a
      )
      state
  _ -> state

errorDisplay :: forall w i. Error -> HH.HTML w i
errorDisplay errors =
  HH.ul_
    ( fromError errors
        # map \msg ->
            HH.li [ HP.classes [ BS.alert, BS.alertDanger ] ] [ HH.text msg ]
    )