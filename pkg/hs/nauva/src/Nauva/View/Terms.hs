{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Nauva.View.Terms
    ( a_
    , area_
    , base_
    , blockquote_
    , br_
    , button_
    , circle_
    , className_
    , code_
    , col_
    , cx_
    , cy_
    , d_
    , div_
    , em_
    , embed_
    , fill_
    , h1_
    , h2_
    , h3_
    , h4_
    , h5_
    , h6_
    , height_
    , hr_
    , href_
    , i_
    , img_
    , input_
    , keygen_
    , li_
    , link_
    , meta_
    , ol_
    , p_
    , param_
    , path_
    , pre_
    , r_
    , rect_
    , ref_
    , section_
    , source_
    , span_
    , src_
    , stroke_
    , strokeWidth_
    , strong_
    , style_
    , svg_
    , track_
    , ul_
    , value_
    , viewBox_
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

blockquote_ :: Term arg res => arg -> res
blockquote_ = term "blockquote"
{-# INLINE blockquote_ #-}

button_ :: Term arg res => arg -> res
button_ = term "button"
{-# INLINE button_ #-}

circle_ :: Term arg res => arg -> res
circle_ = term "circle"
{-# INLINE circle_ #-}

code_ :: Term arg res => arg -> res
code_ = term "code"
{-# INLINE code_ #-}

div_ :: Term arg res => arg -> res
div_ = term "div"
{-# INLINE div_ #-}

em_ :: Term arg res => arg -> res
em_ = term "em"
{-# INLINE em_ #-}

h1_ :: Term arg res => arg -> res
h1_ = term "h1"
{-# INLINE h1_ #-}

h2_ :: Term arg res => arg -> res
h2_ = term "h2"
{-# INLINE h2_ #-}

h3_ :: Term arg res => arg -> res
h3_ = term "h3"
{-# INLINE h3_ #-}

h4_ :: Term arg res => arg -> res
h4_ = term "h4"
{-# INLINE h4_ #-}

h5_ :: Term arg res => arg -> res
h5_ = term "h5"
{-# INLINE h5_ #-}

h6_ :: Term arg res => arg -> res
h6_ = term "h6"
{-# INLINE h6_ #-}

i_ :: Term arg res => arg -> res
i_ = term "i"
{-# INLINE i_ #-}

li_ :: Term arg res => arg -> res
li_ = term "li"
{-# INLINE li_ #-}

ol_ :: Term arg res => arg -> res
ol_ = term "ol"
{-# INLINE ol_ #-}

p_ :: Term arg res => arg -> res
p_ = term "p"
{-# INLINE p_ #-}

path_ :: Term arg res => arg -> res
path_ = term "path"
{-# INLINE path_ #-}

pre_ :: Term arg res => arg -> res
pre_ = term "pre"
{-# INLINE pre_ #-}

rect_ :: Term arg res => arg -> res
rect_ = term "rect"
{-# INLINE rect_ #-}

section_ :: Term arg res => arg -> res
section_ = term "section"
{-# INLINE section_ #-}

span_ :: Term arg res => arg -> res
span_ = term "span"
{-# INLINE span_ #-}

strong_ :: Term arg res => arg -> res
strong_ = term "strong"
{-# INLINE strong_ #-}

style_ :: Term arg res => arg -> res
style_ = term "style"
{-# INLINE style_ #-}

svg_ :: Term arg res => arg -> res
svg_ = term "svg"
{-# INLINE svg_ #-}

ul_ :: Term arg res => arg -> res
ul_ = term "ul"
{-# INLINE ul_ #-}

value_ :: Term arg res => arg -> res
value_ = term "value"
{-# INLINE value_ #-}



className_ :: Term arg res => arg -> res
className_ = term "className"
{-# INLINE className_ #-}

cx_ :: Term arg res => arg -> res
cx_ = term "cx"
{-# INLINE cx_ #-}

cy_ :: Term arg res => arg -> res
cy_ = term "cy"
{-# INLINE cy_ #-}

d_ :: Term arg res => arg -> res
d_ = term "d"
{-# INLINE d_ #-}

fill_ :: Term arg res => arg -> res
fill_ = term "fill"
{-# INLINE fill_ #-}

height_ :: Term arg res => arg -> res
height_ = term "height"
{-# INLINE height_ #-}

href_ :: Term arg res => arg -> res
href_ = term "href"
{-# INLINE href_ #-}

r_ :: Term arg res => arg -> res
r_ = term "r"
{-# INLINE r_ #-}

ref_ :: Term arg res => arg -> res
ref_ = term "ref"
{-# INLINE ref_ #-}

src_ :: Term arg res => arg -> res
src_ = term "src"
{-# INLINE src_ #-}

stroke_ :: Term arg res => arg -> res
stroke_ = term "stroke"
{-# INLINE stroke_ #-}

strokeWidth_ :: Term arg res => arg -> res
strokeWidth_ = term "strokeWidth"
{-# INLINE strokeWidth_ #-}

viewBox_ :: Term arg res => arg -> res
viewBox_ = term "viewBox"
{-# INLINE viewBox_ #-}

width_ :: Term arg res => arg -> res
width_ = term "width"
{-# INLINE width_ #-}

x_ :: Term arg res => arg -> res
x_ = term "x"
{-# INLINE x_ #-}

y_ :: Term arg res => arg -> res
y_ = term "y"
{-# INLINE y_ #-}
