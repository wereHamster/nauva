{-# LANGUAGE OverloadedStrings #-}

module Nauva.Catalog.Theme.Color
    ( black
    ) where


import           Data.Color
import qualified Data.Text as T
import           Data.Monoid

import           Control.Lens

import           Nauva.View


blackColor :: Color
blackColor = mkColor (Chromaticity 0.238 0.281) 0.00424

black :: CSSValue
black = CSSValue $ "rgb(" <> ts r <> "," <> ts g <> "," <> ts b <> ")"
  where
    (r, g, b) = blackColor ^. toSRGB ^. cvSRGB8 ^. to unColorV
    ts = T.pack . show
