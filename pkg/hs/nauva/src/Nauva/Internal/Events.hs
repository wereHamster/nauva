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

data EventHandlerS a = EventHandlerS
    { ehsPreventDefault :: Exp Bool
    , ehsStopPropagation :: Exp Bool
    , ehsStopImmediatePropagation :: Exp Bool
    , ehsAction :: Exp (Maybe a)
    }

type EventHandlerM a = SL.State (EventHandlerS a)


preventDefault :: EventHandlerM a ()
preventDefault = do
    SL.modify $ \s -> s { ehsPreventDefault = litE True }

stopPropagation :: EventHandlerM a ()
stopPropagation = do
    SL.modify $ \s -> s { ehsStopPropagation = litE True }

stopImmediatePropagation :: EventHandlerM a ()
stopImmediatePropagation = do
    SL.modify $ \s -> s { ehsStopImmediatePropagation = litE True }

action :: Exp a -> EventHandlerM a ()
action a = do
    SL.modify $ \s -> s { ehsAction = justE a }

eventHandler :: (Exp ev -> EventHandlerM a ()) -> FE ev a
eventHandler f = createF $ \fId -> F1 fId $ \ev ->
    let (EventHandlerS a b c d) = SL.execState (f ev) emptyEventHandlerS
    in eventHandlerE a b c d
  where
     emptyEventHandlerS = EventHandlerS (litE False) (litE False) (litE False) nothingE





--------------------------------------------------------------------------------
-- | The thing which is attached to nodes. Is a GADT because we need to hide
-- the type of events which the listener processes and the type of actions
-- which it produces.

data EventListener where
    EventListener :: (A.ToJSON a, Value a) => Text -> FE ev a -> EventListener

instance Eq EventListener where
    (EventListener nA fA) == (EventListener nB fB) = nA == nB && f1Id fA == f1Id fB

instance A.ToJSON EventListener where
    toJSON (EventListener ev f) = A.toJSON
        [ A.toJSON (f1Id f)
        , A.String ev
        , A.toJSON (f1Fn f $ holeE 0)
        ]


onClick :: (A.ToJSON r, Value r) => F1 MouseEvent (EventHandler r) -> EventListener
onClick = EventListener "click"

onChange :: (A.ToJSON r, Value r) => F1 MouseEvent (EventHandler r) -> EventListener
onChange = EventListener "change"

onWheel :: (A.ToJSON r, Value r) => F1 WheelEvent (EventHandler r) -> EventListener
onWheel = EventListener "wheel"

onMouseMove :: (A.ToJSON r, Value r) => F1 MouseEvent (EventHandler r) -> EventListener
onMouseMove = EventListener "mouseMove"

onResize :: (A.ToJSON r, Value r) => F1 MouseEvent (EventHandler r) -> EventListener
onResize = EventListener "resize"
