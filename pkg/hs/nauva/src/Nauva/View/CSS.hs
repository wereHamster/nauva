{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Nauva.View.CSS where


import           Data.Text (Text)
import qualified Data.Text as T

import           Nauva.Internal.Types



class CSSTerm a where
    cssTerm :: Text -> a

instance CSSTerm CSSValue where
    cssTerm = CSSValue

instance (a ~ (), v ~ CSSValue) => CSSTerm (v -> StyleM a) where
    cssTerm property value = tellDeclaration property (unCSSValue value)


--------------------------------------------------------------------------------
-- Standard CSS terms which can appear as properties or values.

block :: CSSTerm a => a
block = cssTerm "block"

column :: CSSTerm a => a
column = cssTerm "column"

display :: CSSTerm a => a
display = cssTerm "display"

flex :: CSSTerm a => a
flex = cssTerm "flex"

flexDirection :: CSSTerm a => a
flexDirection = cssTerm "flex-direction"

height :: CSSTerm a => a
height = cssTerm "height"

row :: CSSTerm a => a
row = cssTerm "row"

width :: CSSTerm a => a
width = cssTerm "width"



--------------------------------------------------------------------------------
-- Helper functions to construct values

vh :: Int -> CSSValue
vh n = CSSValue $ T.pack $ (show n) ++ "vh"
