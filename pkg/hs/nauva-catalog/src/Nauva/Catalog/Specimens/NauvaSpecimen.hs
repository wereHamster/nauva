{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}

module Nauva.Catalog.Specimens.NauvaSpecimen
    ( NauvaSpecimen(..)
    , nauvaSpecimen
    ) where


import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.Monoid
import           Data.List
import qualified Data.Aeson         as A

import qualified Text.Blaze.Html    as B
import qualified Text.Blaze.Html.Renderer.Pretty as B

import           Nauva.View
import           Nauva.Catalog.Theme.Typeface
import           Nauva.Static
import           Nauva.Catalog.Specimens.CodeSpecimen
import           Nauva.Catalog.Elements



data NauvaSpecimen = NauvaSpecimen
    { csProps :: CodeSpecimenProps
    , csElement :: Element
    , csLang :: Text
    , csSource :: Text
    }

data State = State
    { nssLang :: Text
    , nssStyles :: [Style]
    , nssHtml :: B.Html
    }

data Action
    = NSASelectLanguage Text

instance Value Action where
    parseValue v = do
        list <- A.parseJSON v
        case list of
            (t:xs) -> do
                ctag <- A.parseJSON t
                case ctag :: Text of
                    "NSASelectLanguage" -> do
                        case xs of
                            [a] -> NSASelectLanguage <$> A.parseJSON a
                            _ -> fail "Action:NSASelectLanguage"
                    _ -> fail "Action"
            _ -> fail "Action"


$( return [] )



nauvaSpecimen :: NauvaSpecimen -> Element
nauvaSpecimen = component_ nauvaSpecimenComponent

nauvaSpecimenComponent :: Component NauvaSpecimen () State Action
nauvaSpecimenComponent = createComponent $ \componentId -> Component
    { componentId = componentId
    , componentDisplayName = "CodeSpecimen"
    , initialComponentState = \props -> do
        (html, styles, _) <- elementToMarkup $ csElement props
        pure (State "Haskell" styles html, [], [])
    , componentEventListeners = const []
    , componentHooks = emptyHooks
    , processLifecycleEvent = \() _ s -> (s, [])
    , receiveProps = \_ s -> pure (s, [], [])
    , update = update
    , renderComponent = render
    , componentSnapshot = \_ -> A.object []
    , restoreComponent = \_ s -> Right (s, [])
    }
  where
    update (NSASelectLanguage t) _ s = (s { nssLang = t}, [])

    onClickHandler :: FE MouseEvent Action
    onClickHandler = [njs| ev => {
        return $NSASelectLanguage(ev.target.innerText)
    }|]


    renderCSSDeclarations :: CSSStyleDeclaration -> T.Text
    renderCSSDeclarations = mconcat . intersperse ";" . map renderDeclaration
      where
        renderDeclaration (k, CSSValue v) = "\n    " <> k <> ": " <> v

    cssRuleSelector :: Text -> Hash -> [Suffix] -> T.Text
    cssRuleSelector name hash suffixes = ".s" <> (if T.null name then "" else ("-" <> name <> "-")) <> unHash hash <> mconcat (map unSuffix suffixes)

    wrapInConditions [] t = t
    wrapInConditions (CMedia x:xs) t = "@media " <> x <> " {" <> wrapInConditions xs t <> "\n}"
    wrapInConditions (CSupports x:xs) t = "@supports " <> x <> " {" <> wrapInConditions xs t <> "\n}"

    renderCSSRule :: CSSRule -> T.Text
    renderCSSRule (CSSStyleRule name hash conditions suffixes styleDeclaration) = wrapInConditions conditions $ mconcat
        [ cssRuleSelector name hash suffixes <> " {"
        , renderCSSDeclarations styleDeclaration
        , "\n}"
        ]
    renderCSSRule (CSSFontFaceRule _hash styleDeclaration) = mconcat
        [ "@font-face {"
        , renderCSSDeclarations styleDeclaration
        , "\n}"
        ]

    render NauvaSpecimen{..} State{..} = if cspNoSource
        then div_ [style_ rootStyle] [pageElementContainer [c]]
        else div_ [style_ rootStyle]
            [ pageElementContainer [c]
            , div_ [style_ tabsStyle]
                [ tab "Haskell"
                , tab "HTML"
                , tab "CSS"
                ]
            , case nssLang of
                "Haskell" -> codeBlock "" s
                "HTML" -> codeBlock "" $ T.pack $ B.renderHtml nssHtml
                "CSS" -> codeBlock "" $ mconcat $ intersperse "\n" $
                    nub $ map renderCSSRule (mconcat $ map unStyle nssStyles)
                _ -> codeBlock "" "other"
            ]
      where
        CodeSpecimenProps{..} = csProps
        c = csElement
        s = csSource

        tab t = div_
            [ onClick_ onClickHandler
            , style_ (if nssLang == t then activeTabStyle else tabStyle)
            ]
            [str_ t]

        rootStyle = mkStyle $ do
            typeface mono12Typeface
            fontStyle "normal"
            fontWeight "400"
            color "rgb(51, 51, 51)"
            display "block"
            width "100%"
            background "rgb(255, 255, 255)"
            border "1px solid rgb(238, 238, 238)"

        tabsStyle = mkStyle $ do
            typeface meta14Typeface
            fontSize "16px"
            display flex
            backgroundColor "rgba(180,180,180,.1)"
            borderTop "1px solid rgb(238, 238, 238)"

        tabStyle = mkStyle $ do
            padding "12px" "20px"
            cursor pointer
            color "rgba(0,0,0,.7)"
            position relative
            userSelect none

            transition "color .2s"

            after $ do
                display block
                content "''"
                position absolute
                bottom "5px"
                left "20px"
                right "20px"
                height "2px"
                backgroundColor "transparent"

                transition "background-color .2s, bottom .2s"

            onHover $ do
                color "rgba(0,0,0,1)"

                after $ do
                    backgroundColor "rgba(0,0,0,.7)"
                    bottom "7px"

        activeTabStyle = mkStyle $ do
            padding "12px" "20px"
            cursor pointer
            color "rgba(0,0,0,1)"
            position relative
            userSelect none

            transition "color .2s"

            after $ do
                display block
                content "''"
                position absolute
                bottom "7px"
                left "20px"
                right "20px"
                height "2px"
                backgroundColor "rgba(0,0,0,.7)"

                transition "background-color .2s, bottom .2s"

