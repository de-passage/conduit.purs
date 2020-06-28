module Data.DefaultPreventable where

import Prelude
import Data.Maybe (Maybe(..))
import Web.Event.Internal.Types (Event)
import Web.UIEvent.MouseEvent as ME

class DefaultPreventable a where
  action :: Event -> Maybe a -> a
  preventDefaults :: forall e. ToEvent e => Maybe a -> e -> Maybe a

defaultPreventDefaults :: forall a e. ToEvent e => DefaultPreventable a => Maybe a -> e -> Maybe a
defaultPreventDefaults ma e = Just $ action (toEvent e) ma

class ToEvent e where
  toEvent :: e -> Event

instance fromMouseEvent :: ToEvent ME.MouseEvent where
  toEvent = ME.toEvent
