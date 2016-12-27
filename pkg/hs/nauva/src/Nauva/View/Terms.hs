{-# LANGUAGE OverloadedStrings #-}

module Nauva.View.Terms
    ( a_
    , area_
    , base_
    , br_
    , button_
    , circle_
    , className_
    , col_
    , cx_
    , cy_
    , div_
    , embed_
    , fill_
    , height_
    , hr_
    , img_
    , input_
    , keygen_
    , link_
    , meta_
    , param_
    , r_
    , rect_
    , ref_
    , source_
    , span_
    , style_
    , svg_
    , track_
    , value_
    , wbr_
    , width_
    , x_
    , y_
    ) where


import Nauva.Internal.Types
import Nauva.View.Types


area_ :: [Attribute] -> Element
area_ = with (ENode "area" [] [])
{-# INLINE area_ #-}

base_ :: [Attribute] -> Element
base_ = with (ENode "base" [] [])
{-# INLINE base_ #-}

br_ :: [Attribute] -> Element
br_ = with (ENode "br" [] [])
{-# INLINE br_ #-}

col_ :: [Attribute] -> Element
col_ = with (ENode "col" [] [])
{-# INLINE col_ #-}

embed_ :: [Attribute] -> Element
embed_ = with (ENode "embed" [] [])
{-# INLINE embed_ #-}

hr_ :: [Attribute] -> Element
hr_ = with (ENode "hr" [] [])
{-# INLINE hr_ #-}

img_ :: [Attribute] -> Element
img_ = with (ENode "img" [] [])
{-# INLINE img_ #-}

input_ :: [Attribute] -> Element
input_ = with (ENode "input" [] [])
{-# INLINE input_ #-}

keygen_ :: [Attribute] -> Element
keygen_ = with (ENode "keygen" [] [])
{-# INLINE keygen_ #-}

link_ :: [Attribute] -> Element
link_ = with (ENode "link" [] [])
{-# INLINE link_ #-}

meta_ :: [Attribute] -> Element
meta_ = with (ENode "meta" [] [])
{-# INLINE meta_ #-}

param_ :: [Attribute] -> Element
param_ = with (ENode "param" [] [])
{-# INLINE param_ #-}

source_ :: [Attribute] -> Element
source_ = with (ENode "source" [] [])
{-# INLINE source_ #-}

track_ :: [Attribute] -> Element
track_ = with (ENode "track" [] [])
{-# INLINE track_ #-}

wbr_ :: [Attribute] -> Element
wbr_ = with (ENode "wbr" [] [])
{-# INLINE wbr_ #-}



a_ :: Term arg res => arg -> res
a_ = term "a"
{-# INLINE a_ #-}

button_ :: Term arg res => arg -> res
button_ = term "button"
{-# INLINE button_ #-}

circle_ :: Term arg res => arg -> res
circle_ = term "circle"
{-# INLINE circle_ #-}

className_ :: Term arg res => arg -> res
className_ = term "className"
{-# INLINE className_ #-}

cx_ :: Term arg res => arg -> res
cx_ = term "cx"
{-# INLINE cx_ #-}

cy_ :: Term arg res => arg -> res
cy_ = term "cy"
{-# INLINE cy_ #-}

div_ :: Term arg res => arg -> res
div_ = term "div"
{-# INLINE div_ #-}

fill_ :: Term arg res => arg -> res
fill_ = term "fill"
{-# INLINE fill_ #-}

height_ :: Term arg res => arg -> res
height_ = term "height"
{-# INLINE height_ #-}

r_ :: Term arg res => arg -> res
r_ = term "r"
{-# INLINE r_ #-}

rect_ :: Term arg res => arg -> res
rect_ = term "rect"
{-# INLINE rect_ #-}

ref_ :: Term arg res => arg -> res
ref_ = term "ref"
{-# INLINE ref_ #-}

span_ :: Term arg res => arg -> res
span_ = term "span"
{-# INLINE span_ #-}

style_ :: Term arg res => arg -> res
style_ = term "style"
{-# INLINE style_ #-}

svg_ :: Term arg res => arg -> res
svg_ = term "svg"
{-# INLINE svg_ #-}

value_ :: Term arg res => arg -> res
value_ = term "value"
{-# INLINE value_ #-}

width_ :: Term arg res => arg -> res
width_ = term "width"
{-# INLINE width_ #-}

x_ :: Term arg res => arg -> res
x_ = term "x"
{-# INLINE x_ #-}

y_ :: Term arg res => arg -> res
y_ = term "y"
{-# INLINE y_ #-}
