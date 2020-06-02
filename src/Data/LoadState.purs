module LoadState where

import Prelude

import Control.Monad.State (class MonadState, modify_)
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)

data LoadState a
  = Loading
  | LoadError String
  | Loaded a

derive instance functorLoadState :: Functor LoadState

instance applyLoadState :: Apply LoadState where
    apply l a = bind l (_ <$> a)

instance bindLoadState :: Bind LoadState where
    bind l f = case l of
        Loading -> Loading
        LoadError e -> LoadError e
        Loaded a -> f a

instance applicativeLoadState :: Applicative LoadState where 
    pure = Loaded

load ::
  forall a s m.
  MonadState s m =>
  MonadAff m =>
  (Aff (Either String a)) ->
  (LoadState a -> s -> s) ->
  m Unit
load get set = do
  result <- liftAff $ get
  case result of
    (Left err) -> modify_ (set (LoadError err))
    (Right arts) -> modify_ (set (Loaded arts))
