{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}

module Nauva.View.Types where


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
