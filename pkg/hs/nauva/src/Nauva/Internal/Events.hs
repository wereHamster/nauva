{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Nauva.Internal.Events where


import qualified Data.Aeson as A
import           Data.Text (Text)

import qualified Control.Monad.State.Lazy as SL

import           Nauva.NJS



--------------------------------------------------------------------------------
-- Type tags for all the different events we want to be able to transform using
-- 'EventListener'.

data WheelEvent    = WheelEvent
data MouseEvent    = MouseEvent
data KeyboardEvent = KeyboardEvent
data Event         = Event




--------------------------------------------------------------------------------
-- | The thing which is attached to nodes. A tuple of @DOM event name@ and the
-- function which shall be used to process the event.

data EventListener = EventListener Text F
    deriving (Eq)

instance A.ToJSON EventListener where
    toJSON (EventListener ev f) = A.toJSON
        [ A.String ev
        , A.toJSON f
        ]


onClick :: F1 MouseEvent r -> EventListener
onClick = EventListener "click"

onChange :: F1 MouseEvent r -> EventListener
onChange = EventListener "change"

onWheel :: F1 WheelEvent r -> EventListener
onWheel = EventListener "wheel"

onMouseMove :: F1 MouseEvent r -> EventListener
onMouseMove = EventListener "mouseMove"

onResize :: F1 MouseEvent r -> EventListener
onResize = EventListener "resize"
