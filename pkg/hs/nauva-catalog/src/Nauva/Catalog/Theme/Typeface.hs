{-# LANGUAGE OverloadedStrings #-}

module Nauva.Catalog.Theme.Typeface
    ( h2Typeface
    , h3Typeface
    , h4Typeface

    , paragraphTypeface
    , blockquoteTypeface
    , meta14Typeface

    , system14Typeface

    , mono12Typeface
    , mono14Typeface
    ) where


import Nauva.View



themeFontFamily :: CSSValue
themeFontFamily = "'Roboto', sans-serif"



-------------------------------------------------------------------------------
-- Headings

h2Typeface :: Typeface
h2Typeface = Typeface
    { tfName       = "h2"
    , tfFontFamily = themeFontFamily
    , tfFontWeight = "400"
    , tfFontSize   = "27.648px"
    , tfLineHeight = "1.2"
    }

h3Typeface :: Typeface
h3Typeface = Typeface
    { tfName       = "h3"
    , tfFontFamily = themeFontFamily
    , tfFontWeight = "400"
    , tfFontSize   = "23.04px"
    , tfLineHeight = "1.2"
    }

h4Typeface :: Typeface
h4Typeface = Typeface
    { tfName       = "h4"
    , tfFontFamily = themeFontFamily
    , tfFontWeight = "400"
    , tfFontSize   = "19.2px"
    , tfLineHeight = "1.2"
    }



-------------------------------------------------------------------------------
-- Copy

paragraphTypeface :: Typeface
paragraphTypeface = Typeface
    { tfName       = "paragraph"
    , tfFontFamily = themeFontFamily
    , tfFontWeight = "400"
    , tfFontSize   = "16px"
    , tfLineHeight = "1.44"
    }

blockquoteTypeface :: Typeface
blockquoteTypeface = Typeface
    { tfName       = "blockquote"
    , tfFontFamily = themeFontFamily
    , tfFontWeight = "400"
    , tfFontSize   = "19.2px"
    , tfLineHeight = "1.44"
    }

meta14Typeface :: Typeface
meta14Typeface = Typeface
    { tfName       = "paragraph"
    , tfFontFamily = themeFontFamily
    , tfFontWeight = "400"
    , tfFontSize   = "14px"
    , tfLineHeight = "1.2"
    }



-------------------------------------------------------------------------------
-- System

systemFontFamily :: CSSValue
systemFontFamily = "-apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, Helvetica, Arial, sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\""


system14Typeface :: Typeface
system14Typeface = Typeface
    { tfName       = "system14"
    , tfFontFamily = systemFontFamily
    , tfFontWeight = "400"
    , tfFontSize   = "14px"
    , tfLineHeight = "1.44"
    }



-------------------------------------------------------------------------------
-- Monospace

monoFontFamily :: CSSValue
monoFontFamily = "'SFMono-Regular', Consolas, 'Liberation Mono', Menlo, Courier, monospace"


mono12Typeface :: Typeface
mono12Typeface = Typeface
    { tfName       = "mono12"
    , tfFontFamily = monoFontFamily
    , tfFontWeight = "400"
    , tfFontSize   = "12px"
    , tfLineHeight = "1.44"
    }

mono14Typeface :: Typeface
mono14Typeface = Typeface
    { tfName       = "mono14"
    , tfFontFamily = monoFontFamily
    , tfFontWeight = "400"
    , tfFontSize   = "14px"
    , tfLineHeight = "1.44"
    }
