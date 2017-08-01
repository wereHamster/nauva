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

import           Nauva.Internal.Types
import           Nauva.Internal.Events
import           Nauva.NJS

import           Nauva.View.Types
import           Nauva.View.Terms



onMouseMove_ :: F1 MouseEvent r -> Attribute
onMouseMove_ = AEVL . EventListener "mouseMove"

onClick_ :: F1 MouseEvent r -> Attribute
onClick_ = AEVL . EventListener "click"

onChange_ :: F1 MouseEvent r -> Attribute
onChange_ = AEVL . EventListener "change"


null_ :: Element
null_ = ENull

str_ :: Text -> Element
str_ = EText

thunk_ :: Term arg res => arg -> res
thunk_ = term "thunk"

component_ :: Term arg res => arg -> res
component_ = term "component"
