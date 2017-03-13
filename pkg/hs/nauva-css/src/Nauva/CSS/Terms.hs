{-# LANGUAGE OverloadedStrings #-}

module Nauva.CSS.Terms
    ( absolute
    , alignItems
    , auto
    , background
    , backgroundColor
    , block
    , border
    , borderBottom
    , borderLeft
    , borderRadius
    , borderRight
    , borderTop
    , bottom
    , boxSizing
    , center
    , color
    , column
    , content
    , cursor
    , display
    , fixed
    , flex
    , flexBasis
    , flexDirection
    , flexEnd
    , flexFlow
    , flexShrink
    , flexWrap
    , fontFamily
    , fontSize
    , fontStyle
    , fontVariant
    , fontWeight
    , height
    , hyphens
    , inlineBlock
    , justifyContent
    , left
    , lineHeight
    , listStyle
    , margin
    , marginBottom
    , marginLeft
    , marginRight
    , marginTop
    , maxHeight
    , maxWidth
    , minHeight
    , none
    , normal
    , opacity
    , outline
    , overflow
    , overflowX
    , overflowY
    , padding
    , paddingBottom
    , paddingLeft
    , paddingRight
    , paddingTop
    , pointer
    , position
    , quotes
    , relative
    , right
    , row
    , src
    , textAlign
    , textDecoration
    , textIndent
    , textRendering
    , textTransform
    , top
    , transform
    , transition
    , uppercase
    , userSelect
    , whiteSpace
    , width
    , wrap
    ) where


import Nauva.CSS.Types


absolute :: CSSTerm a => a
absolute = cssTerm "absolute"
{-# INLINE absolute #-}

alignItems :: CSSTerm a => a
alignItems = cssTerm "align-items"
{-# INLINE alignItems #-}

auto :: CSSTerm a => a
auto = cssTerm "auto"
{-# INLINE auto #-}

background :: CSSTerm a => a
background = cssTerm "background"
{-# INLINE background #-}

backgroundColor :: CSSTerm a => a
backgroundColor = cssTerm "background-color"
{-# INLINE backgroundColor #-}

block :: CSSTerm a => a
block = cssTerm "block"
{-# INLINE block #-}

border :: CSSTerm a => a
border = cssTerm "border"
{-# INLINE border #-}

borderBottom :: CSSTerm a => a
borderBottom = cssTerm "border-bottom"
{-# INLINE borderBottom #-}

borderLeft :: CSSTerm a => a
borderLeft = cssTerm "border-left"
{-# INLINE borderLeft #-}

borderRadius :: CSSTerm a => a
borderRadius = cssTerm "border-radius"
{-# INLINE borderRadius #-}

borderRight :: CSSTerm a => a
borderRight = cssTerm "border-right"
{-# INLINE borderRight #-}

borderTop :: CSSTerm a => a
borderTop = cssTerm "border-top"
{-# INLINE borderTop #-}

bottom :: CSSTerm a => a
bottom = cssTerm "bottom"
{-# INLINE bottom #-}

boxSizing :: CSSTerm a => a
boxSizing = cssTerm "box-sizing"
{-# INLINE boxSizing #-}

center :: CSSTerm a => a
center = cssTerm "center"
{-# INLINE center #-}

color :: CSSTerm a => a
color = cssTerm "color"
{-# INLINE color #-}

column :: CSSTerm a => a
column = cssTerm "column"
{-# INLINE column #-}

content :: CSSTerm a => a
content = cssTerm "content"
{-# INLINE content #-}

cursor :: CSSTerm a => a
cursor = cssTerm "cursor"
{-# INLINE cursor #-}

display :: CSSTerm a => a
display = cssTerm "display"
{-# INLINE display #-}

fixed :: CSSTerm a => a
fixed = cssTerm "fixed"
{-# INLINE fixed #-}

flex :: CSSTerm a => a
flex = cssTerm "flex"
{-# INLINE flex #-}

flexBasis :: CSSTerm a => a
flexBasis = cssTerm "flex-basis"
{-# INLINE flexBasis #-}

flexDirection :: CSSTerm a => a
flexDirection = cssTerm "flex-direction"
{-# INLINE flexDirection #-}

flexEnd :: CSSTerm a => a
flexEnd = cssTerm "flex-end"
{-# INLINE flexEnd #-}

flexFlow :: CSSTerm a => a
flexFlow = cssTerm "flex-flow"
{-# INLINE flexFlow #-}

flexShrink :: CSSTerm a => a
flexShrink = cssTerm "flex-shrink"
{-# INLINE flexShrink #-}

flexWrap :: CSSTerm a => a
flexWrap = cssTerm "flex-wrap"
{-# INLINE flexWrap #-}

fontFamily :: CSSTerm a => a
fontFamily = cssTerm "font-family"
{-# INLINE fontFamily #-}

fontSize :: CSSTerm a => a
fontSize = cssTerm "font-size"
{-# INLINE fontSize #-}

fontStyle :: CSSTerm a => a
fontStyle = cssTerm "font-style"
{-# INLINE fontStyle #-}

fontVariant :: CSSTerm a => a
fontVariant = cssTerm "font-variant"
{-# INLINE fontVariant #-}

fontWeight :: CSSTerm a => a
fontWeight = cssTerm "font-weight"
{-# INLINE fontWeight #-}

height :: CSSTerm a => a
height = cssTerm "height"
{-# INLINE height #-}

hyphens :: CSSTerm a => a
hyphens = cssTerm "hyphens"
{-# INLINE hyphens #-}

inlineBlock :: CSSTerm a => a
inlineBlock = cssTerm "inline-block"
{-# INLINE inlineBlock #-}

justifyContent :: CSSTerm a => a
justifyContent = cssTerm "justify-content"
{-# INLINE justifyContent #-}

left :: CSSTerm a => a
left = cssTerm "left"
{-# INLINE left #-}

lineHeight :: CSSTerm a => a
lineHeight = cssTerm "line-height"
{-# INLINE lineHeight #-}

listStyle :: CSSTerm a => a
listStyle = cssTerm "list-style"
{-# INLINE listStyle #-}

margin :: CSSTerm a => a
margin = cssTerm "margin"
{-# INLINE margin #-}

marginBottom :: CSSTerm a => a
marginBottom = cssTerm "margin-bottom"
{-# INLINE marginBottom #-}

marginLeft :: CSSTerm a => a
marginLeft = cssTerm "margin-left"
{-# INLINE marginLeft #-}

marginRight :: CSSTerm a => a
marginRight = cssTerm "margin-right"
{-# INLINE marginRight #-}

marginTop :: CSSTerm a => a
marginTop = cssTerm "margin-top"
{-# INLINE marginTop #-}

maxHeight :: CSSTerm a => a
maxHeight = cssTerm "max-height"
{-# INLINE maxHeight #-}

maxWidth :: CSSTerm a => a
maxWidth = cssTerm "max-width"
{-# INLINE maxWidth #-}

minHeight :: CSSTerm a => a
minHeight = cssTerm "min-height"
{-# INLINE minHeight #-}

none :: CSSTerm a => a
none = cssTerm "none"
{-# INLINE none #-}

normal :: CSSTerm a => a
normal = cssTerm "normal"
{-# INLINE normal #-}

opacity :: CSSTerm a => a
opacity = cssTerm "opacity"
{-# INLINE opacity #-}

outline :: CSSTerm a => a
outline = cssTerm "outline"
{-# INLINE outline #-}

overflow :: CSSTerm a => a
overflow = cssTerm "overflow"
{-# INLINE overflow #-}

overflowX :: CSSTerm a => a
overflowX = cssTerm "overflow-x"
{-# INLINE overflowX #-}

overflowY :: CSSTerm a => a
overflowY = cssTerm "overflow-y"
{-# INLINE overflowY #-}

padding :: CSSTerm a => a
padding = cssTerm "padding"
{-# INLINE padding #-}

paddingBottom :: CSSTerm a => a
paddingBottom = cssTerm "padding-bottom"
{-# INLINE paddingBottom #-}

paddingLeft :: CSSTerm a => a
paddingLeft = cssTerm "padding-left"
{-# INLINE paddingLeft #-}

paddingRight :: CSSTerm a => a
paddingRight = cssTerm "padding-right"
{-# INLINE paddingRight #-}

paddingTop :: CSSTerm a => a
paddingTop = cssTerm "padding-top"
{-# INLINE paddingTop #-}

pointer :: CSSTerm a => a
pointer = cssTerm "pointer"
{-# INLINE pointer #-}

position :: CSSTerm a => a
position = cssTerm "position"
{-# INLINE position #-}

quotes :: CSSTerm a => a
quotes = cssTerm "quotes"
{-# INLINE quotes #-}

relative :: CSSTerm a => a
relative = cssTerm "relative"
{-# INLINE relative #-}

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

textDecoration :: CSSTerm a => a
textDecoration = cssTerm "text-decoration"
{-# INLINE textDecoration #-}

textIndent :: CSSTerm a => a
textIndent = cssTerm "text-indent"
{-# INLINE textIndent #-}

textRendering :: CSSTerm a => a
textRendering = cssTerm "text-rendering"
{-# INLINE textRendering #-}

textTransform :: CSSTerm a => a
textTransform = cssTerm "text-transform"
{-# INLINE textTransform #-}

top :: CSSTerm a => a
top = cssTerm "top"
{-# INLINE top #-}

transform :: CSSTerm a => a
transform = cssTerm "transform"
{-# INLINE transform #-}

transition :: CSSTerm a => a
transition = cssTerm "transition"
{-# INLINE transition #-}

uppercase :: CSSTerm a => a
uppercase = cssTerm "uppercase"
{-# INLINE uppercase #-}

userSelect :: CSSTerm a => a
userSelect = cssTerm "user-select"
{-# INLINE userSelect #-}

whiteSpace :: CSSTerm a => a
whiteSpace = cssTerm "white-space"
{-# INLINE whiteSpace #-}

width :: CSSTerm a => a
width = cssTerm "width"
{-# INLINE width #-}

wrap :: CSSTerm a => a
wrap = cssTerm "wrap"
{-# INLINE wrap #-}
