{-# LANGUAGE OverloadedStrings #-}

module Nauva.Catalog.Theme.Color
    ( blackColor
    , black

    , red
    , gray
    , lightGray
    ) where


import           Data.Color
import qualified Data.Text as T
import           Data.Monoid

import           Control.Lens

import           Nauva.View



colorCSSValue :: Color -> CSSValue
colorCSSValue c = CSSValue $ "rgb(" <> ts r <> "," <> ts g <> "," <> ts b <> ")"
  where
    (r, g, b) = c ^. toSRGB ^. cvSRGB8 ^. to unColorV
    ts = T.pack . show


blackColor :: Color
blackColor = mkColor (Chromaticity 0.238 0.281) 0.00424

black :: CSSValue
black = colorCSSValue blackColor

red :: CSSValue
red = colorCSSValue $ mkColor (Chromaticity 0.603 0.322) 0.22649

gray :: CSSValue
gray = colorCSSValue $ mkColor (Chromaticity 0.313 0.329) 0.05781

lightGray :: CSSValue
lightGray = colorCSSValue $ mkColor (Chromaticity 0.313 0.329) 0.87962
