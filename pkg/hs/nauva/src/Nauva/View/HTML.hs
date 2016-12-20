{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}

module Nauva.View.HTML where


import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Monoid
import qualified Data.Aeson as A
import           Data.Typeable

import           Nauva.DOM
import           Nauva.Internal.Types
import           Nauva.Internal.Events
import           Nauva.NJS
import           Nauva.CSS


class Term arg res | arg -> res where
    term :: Text -> arg -> res


instance Term Text Attribute where
    term = stringAttribute

instance Term Int Attribute where
    term = intAttribute

instance Term Style Attribute where
    term _ = styleAttribute

instance Term EventListener Attribute where
    term _ = eventListenerAttribute

instance Term Ref Attribute where
    term _ = refAttribute


instance Term [Attribute] ([Element] -> Element) where
    term tag = ENode (Tag tag)

instance Term [Element] Element where
    term tag = ENode (Tag tag) []

instance (Typeable p) => Term (Thunk p) (p -> Element) where
    term _ = EThunk

instance (Typeable p, A.FromJSON a, Value h, Value a) => Term (Component p h s a) (p -> Element) where
    term _ = EComponent


class With a where
    with :: a -> [Attribute] -> a

instance With Element where
    with (ENode tag attrs children) extraAttrs = ENode tag (attrs <> extraAttrs) children
    with el                         _          = el

instance With ([Element] -> Element) where
    with f extraAttrs = \arg -> case f arg of
        (ENode tag attrs children) -> ENode tag (attrs <> extraAttrs) children
        el                         -> el


div_ :: Term arg res => arg -> res
div_ = term "div"

span_ :: Term arg res => arg -> res
span_ = term "span"

button_ :: Term arg res => arg -> res
button_ = term "button"

input_ :: Term arg res => arg -> res
input_ = term "input"

circle_ :: Term arg res => arg -> res
circle_ = term "circle"

svg_ :: Term arg res => arg -> res
svg_ = term "svg"

rect_ :: Term arg res => arg -> res
rect_ = term "rect"

style_ :: Term arg res => arg -> res
style_ = term "style"

value_ :: Term arg res => arg -> res
value_ = term "value"

width_ :: Term arg res => arg -> res
width_ = term "width"

height_ :: Term arg res => arg -> res
height_ = term "height"

r_ :: Term arg res => arg -> res
r_ = term "r"

x_ :: Term arg res => arg -> res
x_ = term "x"

y_ :: Term arg res => arg -> res
y_ = term "y"

cx_ :: Term arg res => arg -> res
cx_ = term "cx"

cy_ :: Term arg res => arg -> res
cy_ = term "cy"

fill_ :: Term arg res => arg -> res
fill_ = term "fill"

ref_ :: Term arg res => arg -> res
ref_ = term "ref"

className_ :: Term arg res => arg -> res
className_ = term "className"

br_ :: [Attribute] -> Element
br_ = with (ENode "br" [] [])


onMouseMove_ :: (A.ToJSON r, Value r) => F1 MouseEvent (EventHandler r) -> Attribute
onMouseMove_ = AEVL . EventListener "mouseMove"

onClick_ :: (A.ToJSON r, Value r) => F1 MouseEvent (EventHandler r) -> Attribute
onClick_ = AEVL . EventListener "click"

onChange_ :: (A.ToJSON r, Value r) => F1 MouseEvent (EventHandler r) -> Attribute
onChange_ = AEVL . EventListener "change"


str_ :: Text -> Element
str_ = EText

thunk_ :: Term arg res => arg -> res
thunk_ = term "thunk"

component_ :: Term arg res => arg -> res
component_ = term "component"
