{-# LANGUAGE OverloadedStrings #-}

module Nauva.View.CSS.Terms
    ( block
    , column
    , display
    , flex
    , flexDirection
    , height
    , row
    , width
    ) where


import Nauva.View.CSS.Internal


block :: CSSTerm a => a
block = cssTerm "block"
{-# INLINE block #-}

column :: CSSTerm a => a
column = cssTerm "column"
{-# INLINE column #-}

display :: CSSTerm a => a
display = cssTerm "display"
{-# INLINE display #-}

flex :: CSSTerm a => a
flex = cssTerm "flex"
{-# INLINE flex #-}

flexDirection :: CSSTerm a => a
flexDirection = cssTerm "flex-direction"
{-# INLINE flexDirection #-}

height :: CSSTerm a => a
height = cssTerm "height"
{-# INLINE height #-}

row :: CSSTerm a => a
row = cssTerm "row"
{-# INLINE row #-}

width :: CSSTerm a => a
width = cssTerm "width"
{-# INLINE width #-}
