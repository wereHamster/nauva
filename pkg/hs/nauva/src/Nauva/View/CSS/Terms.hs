{-# LANGUAGE OverloadedStrings #-}

module Nauva.View.CSS.Terms
    ( backgroundColor
    , block
    , center
    , color
    , column
    , display
    , flex
    , flexDirection
    , fontFamily
    , fontSize
    , height
    , lineHeight
    , margin
    , marginLeft
    , marginRight
    , right
    , row
    , textAlign
    , width
    ) where


import Nauva.View.CSS.Internal


backgroundColor :: CSSTerm a => a
backgroundColor = cssTerm "background-color"
{-# INLINE backgroundColor #-}

block :: CSSTerm a => a
block = cssTerm "block"
{-# INLINE block #-}

center :: CSSTerm a => a
center = cssTerm "center"
{-# INLINE center #-}

color :: CSSTerm a => a
color = cssTerm "color"
{-# INLINE color #-}

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

fontFamily :: CSSTerm a => a
fontFamily = cssTerm "font-family"
{-# INLINE fontFamily #-}

fontSize :: CSSTerm a => a
fontSize = cssTerm "font-size"
{-# INLINE fontSize #-}

height :: CSSTerm a => a
height = cssTerm "height"
{-# INLINE height #-}

lineHeight :: CSSTerm a => a
lineHeight = cssTerm "line-height"
{-# INLINE lineHeight #-}

margin :: CSSTerm a => a
margin = cssTerm "margin"
{-# INLINE margin #-}

marginLeft :: CSSTerm a => a
marginLeft = cssTerm "margin-left"
{-# INLINE marginLeft #-}

marginRight :: CSSTerm a => a
marginRight = cssTerm "margin-right"
{-# INLINE marginRight #-}

right :: CSSTerm a => a
right = cssTerm "right"
{-# INLINE right #-}

row :: CSSTerm a => a
row = cssTerm "row"
{-# INLINE row #-}

textAlign :: CSSTerm a => a
textAlign = cssTerm "text-align"
{-# INLINE textAlign #-}

width :: CSSTerm a => a
width = cssTerm "width"
{-# INLINE width #-}
