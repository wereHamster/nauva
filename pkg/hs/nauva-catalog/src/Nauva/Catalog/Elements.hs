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
    ) where


import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.Monoid
import           Data.Typeable
import           Data.Data
import qualified Data.Aeson as A

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
