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
-- | The thing which is attached to nodes. Is a GADT because we need to hide
-- the type of events which the listener processes and the type of actions
-- which it produces.

data EventListener where
    EventListener :: Text -> FE ev a -> EventListener

instance Eq EventListener where
    (EventListener nA fA) == (EventListener nB fB) = nA == nB && fId fA == fId fB

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
