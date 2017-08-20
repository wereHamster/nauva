{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift         #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE TemplateHaskell    #-}

module Nauva.Catalog.Elements
    ( pageRoot

    , pageH2
    , pageH3
    , pageH4

    , pageUL
    , pageOL

    , pageParagraph
    , pageBlockquote
    , pageCodeBlock
    , pageElementContainer
    , pageHint
    , pageCode

    , PageElementProps(..)
    , pageElement

    , codeBlock

    , ColorGroup(..)
    , ColorCell(..)
    , ColorCellValue(..)
    , colorGroup
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

import           Language.Haskell.TH.Syntax

import           Nauva.View
import           Nauva.Catalog.Theme.Typeface



pageRoot :: [Element] -> Element
pageRoot c = div_ [style_ outerStyle] [div_ [style_ innerStyle] c]
  where
    outerStyle = mkStyle $
        margin "0 30px"

    innerStyle = mkStyle $ do
        marginLeft "-16px"
        display flex
        flexFlow row wrap
        padding "48px 0px"



pageH2 :: [Element] -> Element
pageH2 = h2_ [style_ style]
  where
    style = mkStyle $ do
        typeface h2Typeface
        fontStyle "normal"
        color "#003B5C"
        flexBasis "100%"
        margin "48px 0 0 0"
        paddingLeft "16px"

        firstChild $
            marginTop "0"
        lastChild $
            marginBottom "0"



pageH3 :: [Element] -> Element
pageH3 = h3_ [style_ style]
  where
    style = mkStyle $ do
        typeface h3Typeface
        fontStyle "normal"
        color "#003B5C"
        flexBasis "100%"
        margin "48px 0 0 0"
        paddingLeft "16px"

        firstChild $
            marginTop "0"
        lastChild $
            marginBottom "0"


pageH4 :: [Element] -> Element
pageH4 = h4_ [style_ style]
  where
    style = mkStyle $ do
        typeface h4Typeface
        fontStyle "normal"
        color "#003B5C"
        flexBasis "100%"
        margin "16px 0 0 0"
        paddingLeft "16px"

        firstChild $
            marginTop "0"
        lastChild $
            marginBottom "0"


pageParagraph :: Bool -> [Element] -> Element
pageParagraph isTopLevel = if isTopLevel
    then \c -> div_ [style_ full] [p_ [style_ style] c]
    else p_ [style_ style]
  where
    full = mkStyle $ do
        flexBasis "100%"
        paddingLeft "16px"
        margin "8px 0"

        firstChild $
            marginTop "0"
        lastChild $
            marginBottom "0"


    style = mkStyle $ do
        typeface paragraphTypeface
        fontStyle "normal"
        color "#333333"
        flexBasis "100%"
        maxWidth "64em"
        margin "8px 0"

        firstChild $
            marginTop "0"
        lastChild $
            marginBottom "0"


pageBlockquote :: [Element] -> Element
pageBlockquote = blockquote_ [style_ style]
  where
    style = mkStyle $ do
        typeface blockquoteTypeface
        quotes none
        margin "48px 0 32px -20px"
        padding "0 0 0 20px"
        borderLeft "1px solid #D6D6D6"

        -- blockquote > p
        fontStyle "normal"
        color "#333333"
        flexBasis "100%"
        -- margin "16px 0 0 0"

codeBlock :: Text -> Text -> Element
codeBlock lang s = div_ [style_ rootStyle]
    [ if T.null lang then null_ else div_ [style_ langStyle] [str_ lang]
    , pre_ [style_ preStyle] [code_ [style_ codeStyle] [str_ s]]
    ]
  where
    rootStyle = mkStyle $
        position "relative"

    langStyle = mkStyle $ do
        position "absolute"
        fontSize "13px"
        color "#666"
        top "0px"
        left "0px"
        width "100%"
        background "rgba(180,180,180,.1)"
        padding "0px 8px"
        height "30px"
        lineHeight "30px"
        borderTop "1px solid rgb(238, 238, 238)"

    codeStyle = mkStyle $ do
        typeface mono12Typeface

    preStyle = mkStyle $ do
        color "rgb(0, 38, 62)"
        background "rgb(255, 255, 255)"
        border "none"
        display "block"
        height "auto"
        margin "0px"
        overflow "auto"
        if T.null lang
            then padding "20px"
            else padding "50px 20px 20px"
        whiteSpace "pre"
        width "100%"

pageCodeBlock :: Text -> Element
pageCodeBlock s = div_ [style_ pageCodeBlockStyle]
    [ section_ [style_ sectionStyle]
        [ codeBlock "Haskell" s
        ]
    ]
  where
    pageCodeBlockStyle = mkStyle $ do
        display "flex"
        flexBasis "100%"
        maxWidth "100%"
        flexWrap "wrap"
        margin "8px 0"
        paddingLeft "16px"
        position "relative"

    sectionStyle = mkStyle $ do
        typeface mono12Typeface
        fontStyle "normal"
        color "rgb(51, 51, 51)"
        display "block"
        width "100%"
        background "rgb(255, 255, 255)"
        border "1px solid rgb(238, 238, 238)"


pageElementContainer :: [Element] -> Element
pageElementContainer = div_ [style_ style]
  where
    style = mkStyle $ do
        background "url(data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAoAAAAKCAAAAACoWZBhAAAAF0lEQVQI12P4BAI/QICBFCaYBPNJYQIAkUZftTbC4sIAAAAASUVORK5CYII=)"
        borderRadius "2px"
        border none
        display block
        padding "20px"
        position relative
        width "100%"


pageHint :: [Element] -> Element
pageHint c = div_ [style_ pageHintContainerStyle] [div_ [style_ pageHintDefaultStyle] c]
  where
    pageHintDefaultStyle = pageHintStyle "rgb(255, 180, 0)" "rgb(255, 246, 221)" "rgb(255, 239, 170)"
    -- pageHintDirectiveStyle = pageHintStyle "rgb(47, 191, 98)" "rgb(234, 250, 234)" "rgb(187, 235, 200)"

    pageHintContainerStyle = mkStyle $
        paddingLeft "16px"

    pageHintStyle fg bg b = mkStyle $ do
        fontStyle "normal"
        color fg
        background bg
        border $ CSSValue $ "1px solid " <> b
        borderRadius "2px"
        padding "20px"
        flexBasis "100%"
        margin "16px 0"



pageCode :: [Element] -> Element
pageCode = code_ [style_ style]
  where
    style = mkStyle $ do
        typeface mono12Typeface
        background "#F2F2F2"
        border "1px solid #eee"
        borderRadius "1px"
        display inlineBlock
        padding "0.12em 0.2em"
        textIndent "0"


pageUL :: [Element] -> Element
pageUL = ul_ [style_ style]
  where
    style = mkStyle $ do
        typeface paragraphTypeface
        width "100%"
        paddingLeft (px (16 + 32))
        fontStyle "normal"
        color "#333333"
        listStyle "disc"
        margin "16px 0"
        maxWidth "64em"

pageOL :: [Element] -> Element
pageOL = ol_ [style_ style]
  where
    style = mkStyle $ do
        width "100%"
        marginLeft "0"
        paddingLeft "2rem"
        fontStyle "normal"
        fontWeight "400"
        -- textRendering "optimizeLegibility"
        -- -webkit-font-smoothing "antialiased"
        -- -moz-osx-font-smoothing "grayscale"
        color "#333333"
        fontFamily "'Roboto', sans-serif"
        fontSize "16px"
        lineHeight "1.44"
        listStyle "disc"
        marginTop "16px"
        marginBottom "0"
        maxWidth "64em"


data PageElementProps = PageElementProps
    { pepTitle :: Maybe String
    , pepSpan :: Int
    } deriving (Typeable, Data, Lift)

instance A.FromJSON PageElementProps where
    parseJSON (A.Object o) = PageElementProps
        <$> o A..:? "title"
        <*> o A..:? "span" A..!= 6

    parseJSON _ = fail "PageElementProps"

pageElement :: PageElementProps -> [Element] -> Element
pageElement PageElementProps{..} c = case pepTitle of
    Nothing -> div_ [style_ style] c
    Just title -> div_ [style_ style] ([div_ [style_ titleStyle] [pageH4 [str_ (T.pack title)]]] <> c)
  where
    style = mkStyle $ do
        maxWidth "100%"
        margin "12px 0"
        position "relative"
        paddingLeft "16px"
        flexBasis $ cssTerm $ T.pack $ "calc((100% / 6 * " <> show pepSpan <> "))"
        overflow "hidden"

    titleStyle = mkStyle $
        margin "0 0 8px -16px"


-------------------------------------------------------------------------------
-- colorGroup

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

data ColorGroup = ColorGroup
    { cgLabel :: !Text
    , cgCells :: ![ColorCell]
    }


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
