{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift         #-}

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

    , CodeSpecimenProps(..)
    , codeSpecimen

    , codeBlock
    ) where


import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.Monoid
import           Data.Aeson
import           Data.Typeable
import           Data.Data

import          Language.Haskell.TH.Syntax

import           Nauva.Internal.Types
import           Nauva.View


pageRoot :: [Element] -> Element
pageRoot c = div_ [style_ outerStyle] [div_ [style_ innerStyle] c]
  where
    outerStyle = mkStyle $ do
        margin "0 30px 0 40px"

    innerStyle = mkStyle $ do
        marginLeft "-16px"
        maxWidth "64em"
        display flex
        flexFlow row wrap
        padding "48px 0px"



pageH2 :: [Element] -> Element
pageH2 = h2_ [style_ style]
  where
    style = mkStyle $ do
        fontStyle "normal"
        fontWeight "400"
        color "#003B5C"
        fontFamily "'Roboto', sans-serif"
        fontSize "27.648px"
        lineHeight "1.2"
        flexBasis "100%"
        margin "48px 0 0 0"
        paddingLeft "16px"


pageH3 :: [Element] -> Element
pageH3 = h3_ [style_ style]
  where
    style = mkStyle $ do
        fontStyle "normal"
        fontWeight "400"
        color "#003B5C"
        fontFamily "'Roboto', sans-serif"
        fontSize "23.04px"
        lineHeight "1.2"
        flexBasis "100%"
        margin "48px 0 0 0"
        paddingLeft "16px"


pageH4 :: [Element] -> Element
pageH4 = h4_ [style_ style]
  where
    style = mkStyle $ do
        fontStyle "normal"
        fontWeight "400"
        color "#003B5C"
        fontFamily "'Roboto', sans-serif"
        fontSize "19.2px"
        lineHeight "1.2"
        flexBasis "100%"
        margin "16px 0 0 0"
        paddingLeft "16px"


pageParagraph :: [Element] -> Element
pageParagraph = p_ [style_ style]
  where
    style = mkStyle $ do
        fontStyle "normal"
        fontWeight "400"
        color "#333333"
        fontFamily "'Roboto', sans-serif"
        fontSize "16px"
        lineHeight "1.44"
        flexBasis "100%"
        margin "16px 0 0 0"
        paddingLeft "16px"



pageBlockquote :: [Element] -> Element
pageBlockquote = blockquote_ [style_ style]
  where
    style = mkStyle $ do
        quotes none
        margin "48px 0 32px -20px"
        padding "0 0 0 20px"
        borderLeft "1px solid #D6D6D6"

        -- blockquote > p
        fontStyle "normal"
        fontWeight "400"
        color "#333333"
        fontFamily "'Roboto', sans-serif"
        fontSize "19.2px"
        lineHeight "1.44"
        flexBasis "100%"
        -- margin "16px 0 0 0"

codeBlock :: Text -> Text -> Element
codeBlock lang s = div_ [style_ rootStyle]
    [ div_ [style_ langStyle] [str_ lang]
    , pre_ [style_ preStyle] [code_ [style_ codeStyle] [str_ s]]
    ]
  where
    rootStyle = mkStyle $ do
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
        cssTerm "border-top" "1px solid rgb(238, 238, 238)"

    codeStyle = mkStyle $ do
        fontFamily "'Source Code Pro', monospace"
        fontWeight "400"

    preStyle = mkStyle $ do
        fontStyle "normal"
        fontWeight "400"
        color "rgb(0, 38, 62)"
        fontFamily "Roboto, sans-serif"
        fontSize "14.6059px"
        lineHeight "1.44"
        background "rgb(255, 255, 255)"
        border "none"
        display "block"
        height "auto"
        margin "0px"
        overflow "auto"
        padding "50px 20px 20px"
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
        margin "24px 0px 0px"
        padding "0"
        position "relative"

    sectionStyle = mkStyle $ do
        fontStyle "normal"
        fontWeight "400"
        color "rgb(51, 51, 51)"
        fontFamily "'Source Code Pro', monospace"
        fontSize "14.6059px"
        lineHeight "1.44"
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
pageHint = div_ [style_ pageHintDefaultStyle]
  where
    pageHintDefaultStyle = pageHintStyle "rgb(255, 180, 0)" "rgb(255, 246, 221)" "rgb(255, 239, 170)"
    -- pageHintDirectiveStyle = pageHintStyle "rgb(47, 191, 98)" "rgb(234, 250, 234)" "rgb(187, 235, 200)"

    pageHintStyle fg bg b = mkStyle $ do
        fontStyle "normal"
        fontWeight "400"
        color fg
        fontFamily "Roboto, sans-serif"
        fontSize "16px"
        lineHeight "1.44"
        background bg
        border $ CSSValue $ "1px solid " <> b
        borderRadius "2px"
        padding "20px"
        flexBasis "100%"



pageCode :: [Element] -> Element
pageCode = code_ [style_ style]
  where
    style = mkStyle $ do
        background "#F2F2F2"
        border "1px solid #eee"
        borderRadius "1px"
        display inlineBlock
        fontFamily "'Source Code Pro', monospace"
        lineHeight "1"
        padding "0.12em 0.2em"
        cssTerm "text-indent" "0"


pageUL :: [Element] -> Element
pageUL = ul_ [style_ style]
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


data PageElementProps = PageElementProps
    { pepTitle :: Maybe String
    , pepSpan :: Int
    } deriving (Typeable, Data, Lift)

instance FromJSON PageElementProps where
    parseJSON (Object o) = PageElementProps
        <$> o .:? "title"
        <*> o .:? "span" .!= 6

    parseJSON _ = fail "PageElementProps"

pageElement :: PageElementProps -> [Element] -> Element
pageElement (PageElementProps {..}) c = case pepTitle of
    Nothing -> div_ [style_ style] c
    Just title -> div_ [style_ style] ([div_ [style_ titleStyle] [pageH4 [str_ (T.pack title)]]] <> c)
  where
    style = mkStyle $ do
        maxWidth "100%"
        margin "24px 0 0 0"
        position "relative"
        paddingLeft "16px"
        flexBasis $ cssTerm $ T.pack $ "calc((100% / 6 * " <> show pepSpan <> "))"

    titleStyle = mkStyle $ do
        margin "0 0 8px -16px"



data CodeSpecimenProps = CodeSpecimenProps
    { cspPEP :: PageElementProps
    , cspNoSource :: Bool
    } deriving (Typeable, Data, Lift)

instance FromJSON CodeSpecimenProps where
    parseJSON v@(Object o) = CodeSpecimenProps
        <$> parseJSON v
        <*> o .:? "noSource" .!= False

    parseJSON _ = fail "CodeSpecimenProps"


codeSpecimen :: CodeSpecimenProps -> Element -> Text -> Text -> Element
codeSpecimen (CodeSpecimenProps {..}) c lang s = if cspNoSource
    then div_ [style_ rootStyle] [pageElementContainer [c]]
    else div_ [style_ rootStyle] [pageElementContainer [c], codeBlock lang s]
  where
    rootStyle = mkStyle $ do
        fontStyle "normal"
        fontWeight "400"
        color "rgb(51, 51, 51)"
        fontFamily "'Source Code Pro', monospace"
        fontSize "14.6059px"
        lineHeight "1.44"
        display "block"
        width "100%"
        background "rgb(255, 255, 255)"
        border "1px solid rgb(238, 238, 238)"
