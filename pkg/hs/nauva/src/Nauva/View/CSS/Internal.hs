{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module Nauva.View.CSS.Internal
    ( CSSTerm(..)
    ) where


import           Data.Text (Text)
import qualified Data.Text as T

import           Nauva.Internal.Types


class CSSTerm a where
    cssTerm :: Text -> a

instance CSSTerm CSSValue where
    cssTerm = CSSValue

instance (a ~ (), v ~ CSSValue) => CSSTerm (v -> StyleM a) where
    cssTerm property value = tellDeclaration property (unCSSValue value)
