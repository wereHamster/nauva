{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}

module Nauva.Catalog.Specimens.ColorGroupSpecimen
    ( ColorGroup(..)
    , colorGroup

    , ColorCell(..)
    , ColorCellValue(..)
    ) where


import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.Monoid
import           Data.Typeable
import           Data.Data
import           Data.Color
import           Data.Word
import qualified Data.Aeson as A

import           Control.Lens hiding (none)
import           Control.Applicative

import           Language.Haskell.TH.Syntax

import           Nauva.View
import           Nauva.Catalog.Theme.Typeface
import           Nauva.Catalog.Elements



data ColorCellValue = ColorCellValue
    { csvName :: !Text
    , csvColor :: !Color
    }

data ColorCell = ColorCell
    { ccLabel :: !Text
      -- ^ The label of the cell. Shown inside the reference color box.
      -- Can be empty.
    , ccLuminance :: !Double -- 0..100
      -- ^ The luminance of the reference box on the left side of the color cell.
    , ccValue :: !(Maybe ColorCellValue)
    }

-- instance A.FromJSON ColorCell where
--     parseJSON v@(A.Object o) = do
--         luminance <- o A..: "luminance"
--         label <- o A..: "label" <|> (pure $ T.pack $ show luminance)
--         value <-  o A..: "value"

--         pure $ ColorCell label luminance value

data ColorGroup = ColorGroup
    { cgLabel :: !Text
    , cgCells :: ![ColorCell]
    }

-- instance A.FromJSON ColorGroup where
--     parseJSON v@(A.Object o) = ColorGroup
--         <$> o A..: "label"
--         <*> o A..: "cells"


luminanceToRGB :: Double -> (Word8, Word8, Word8)
luminanceToRGB l = CIELAB l 0 0 ^.re (toCIELAB d65) ^. toSRGB ^. cvSRGB8 ^. to unColorV

colorCSSValue :: Getter Color CSSValue
colorCSSValue = to (\c -> CSSValue $ T.pack $ "rgb" <> show (c ^. toSRGB ^. cvSRGB8 ^. to unColorV))


colorGroup :: ColorGroup -> Element
colorGroup ColorGroup{..} = pageElement
    PageElementProps{ pepTitle = Just (T.unpack cgLabel), pepSpan = 1 }
    [div_ [style_ rootStyle] (map colorCell cgCells)]
  where
    rootStyle = mkStyle $ do
        display flex
        flexDirection column
        fontFamily "Roboto"


colorCell :: ColorCell -> Element
colorCell cc@ColorCell{..} = div_ [style_ rootStyle]
    [ div_ [style_ refStyle] [str_ ccLabel]
    , maybe null_ (colorCellValue cc) ccValue
    ]
  where
    rootStyle = mkStyle $ do
        position relative
        display flex
        height (px 56)
        marginBottom "2px"
        alignItems center
        color $ if ccLuminance > 50 then "rgba(0,0,0,.9)" else "rgba(255,255,255,.9)"

    refStyle = mkStyle $ do
        width (px 48)
        flexShrink "0"
        height "56px"
        display flex
        alignItems center
        justifyContent center
        position relative

        let mapTuple f (a, b, c) = (f a, f b, f c)
        -- let v = T.pack (show (floor $ ((100 - ccLuminance) / 100) * 255.0))
        let (r,g,b) = mapTuple (T.pack . show) (luminanceToRGB ccLuminance)

        color $ if ccLuminance > 50 then "rgba(0,0,0,.9)" else "rgba(255,255,255,.9)"
        backgroundColor $ CSSValue $ "rgb(" <> r <> "," <> g <> "," <> b <> ")"


colorCellValue :: ColorCell -> ColorCellValue -> Element
colorCellValue ColorCell{..} ColorCellValue{..} = div_ [style_ colorCellValueStyle]
    [ div_ [style_ colorNameStyle] [str_ csvName]
    , div_ [style_ colorValueStyle] [str_ (unCSSValue (csvColor ^. colorCSSValue))]
    , luminanceDistance ccLuminance l
    ]
  where
    colorCellValueStyle = mkStyle $ do
        flex "1"
        display flex
        justifyContent center
        flexDirection column
        height "56px"
        padding "0" (px 8)
        backgroundColor (csvColor ^. colorCSSValue)
        color $ if l > 50 then "rgba(0,0,0,.9)" else "rgba(255,255,255,.9)"

    colorNameStyle = mkStyle $ do
        marginBottom "4px"

    colorValueStyle = mkStyle $ do
        fontSize "0.8125em"
        color $ if l > 50 then "rgba(0,0,0,.6)" else "rgba(255,255,255,.6)"

    luminanceDistance b a = case round (a - b) of
        0 -> ENull
        d -> div_ [style_ luminanceDistanceStyle] [str_ $ "L" <> (if d > 0 then "+" else "") <> T.pack (show d)]

    luminanceDistanceStyle = mkStyle $ do
        position absolute
        bottom "2px"
        right "3px"
        fontSize "11px"
        lineHeight "11px"

    l = csvColor ^. toCIELAB d65 ^. cvCIELAB ^. to unColorV ^. _1
