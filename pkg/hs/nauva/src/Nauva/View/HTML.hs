{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}

module Nauva.View.HTML
    ( module Nauva.View.Types
    , module Nauva.View.Terms

    , onMouseMove_
    , onClick_
    , onChange_

    , null_
    , str_
    , thunk_
    , component_
    ) where


import           Data.Text (Text)
import qualified Data.Aeson as A

import           Nauva.Internal.Types
import           Nauva.Internal.Events
import           Nauva.NJS

import           Nauva.View.Types
import           Nauva.View.Terms



onMouseMove_ :: (Value r) => F1 MouseEvent (EventHandler r) -> Attribute
onMouseMove_ = AEVL . EventListener "mouseMove"

onClick_ :: (Value r) => F1 MouseEvent (EventHandler r) -> Attribute
onClick_ = AEVL . EventListener "click"

onChange_ :: (Value r) => F1 MouseEvent (EventHandler r) -> Attribute
onChange_ = AEVL . EventListener "change"


null_ :: Element
null_ = ENull

str_ :: Text -> Element
str_ = EText

thunk_ :: Term arg res => arg -> res
thunk_ = term "thunk"

component_ :: Term arg res => arg -> res
component_ = term "component"
