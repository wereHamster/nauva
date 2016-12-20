{-# LANGUAGE OverloadedStrings #-}

module Nauva.CSS.Terms
    ( alignItems
    , auto
    , backgroundColor
    , block
    , border
    , center
    , color
    , column
    , cursor
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
    , none
    , outline
    , padding
    , pointer
    , right
    , row
    , src
    , textAlign
    , width
    ) where


import Nauva.CSS.Types


alignItems :: CSSTerm a => a
alignItems = cssTerm "align-items"
{-# INLINE alignItems #-}

auto :: CSSTerm a => a
auto = cssTerm "auto"
{-# INLINE auto #-}

backgroundColor :: CSSTerm a => a
backgroundColor = cssTerm "background-color"
{-# INLINE backgroundColor #-}

block :: CSSTerm a => a
block = cssTerm "block"
{-# INLINE block #-}

border :: CSSTerm a => a
border = cssTerm "border"
{-# INLINE border #-}

center :: CSSTerm a => a
center = cssTerm "center"
{-# INLINE center #-}

color :: CSSTerm a => a
color = cssTerm "color"
{-# INLINE color #-}

column :: CSSTerm a => a
column = cssTerm "column"
{-# INLINE column #-}

cursor :: CSSTerm a => a
cursor = cssTerm "cursor"
{-# INLINE cursor #-}

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

none :: CSSTerm a => a
none = cssTerm "none"
{-# INLINE none #-}

outline :: CSSTerm a => a
outline = cssTerm "outline"
{-# INLINE outline #-}

padding :: CSSTerm a => a
padding = cssTerm "padding"
{-# INLINE padding #-}

pointer :: CSSTerm a => a
pointer = cssTerm "pointer"
{-# INLINE pointer #-}

right :: CSSTerm a => a
right = cssTerm "right"
{-# INLINE right #-}

row :: CSSTerm a => a
row = cssTerm "row"
{-# INLINE row #-}

src :: CSSTerm a => a
src = cssTerm "src"
{-# INLINE src #-}

textAlign :: CSSTerm a => a
textAlign = cssTerm "text-align"
{-# INLINE textAlign #-}

width :: CSSTerm a => a
width = cssTerm "width"
{-# INLINE width #-}
