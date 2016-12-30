{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE NamedFieldPuns        #-}

module Nauva.Catalog.Shell
    ( HeaderProps(..)
    , header

    , SidebarProps(..)
    , sidebar
    ) where


import           Data.ByteString    (ByteString)
import qualified Data.ByteString    as BS
import           Data.Text          (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Aeson         as A


import           Nauva.DOM
import           Nauva.Internal.Types
import           Nauva.Internal.Events
import           Nauva.NJS
import           Nauva.View

import           Nauva.Service.Router

import           Nauva.Catalog.Types



-------------------------------------------------------------------------------
-- header

data HeaderProps = HeaderProps
    { section :: !Text
    , title :: !Text
    } deriving (Eq)

header :: HeaderProps -> Element
header = thunk_ headerThunk

headerThunk :: Thunk HeaderProps
headerThunk = Thunk
    { thunkId = mkThunkId
    , thunkDisplayName = "Header"
    , shouldThunkUpdate = (==)
    , forceThunk = \(HeaderProps {..}) -> div_ [style_ headerStyle]
        [ h2_ [style_ headerSectionStyle] [str_ section]
        , h1_ [style_ headerTitleStyle] [str_ title]
        ]
    }
  where
    headerStyle = mkStyle $ do
        height (px 200)
        backgroundColor "rgb(0, 59, 92)"
        padding (px 20) (px 40)

        display flex
        flexDirection column
        justifyContent "flex-end"

    headerSectionStyle = mkStyle $ do
        fontFamily "Roboto, sans-serif"
        fontStyle normal
        fontWeight "400"
        color "white"
        fontSize (px 19.2)
        lineHeight "1.2"
        opacity "0.6"
        margin "0"

    headerTitleStyle = mkStyle $ do
        fontFamily "Roboto, sans-serif"
        fontStyle normal
        fontWeight "400"
        color "white"
        fontSize (px 33.1776)
        lineHeight "1.2"
        margin "0"



-------------------------------------------------------------------------------
-- sidebar

data SidebarProps = SidebarProps
    { routerH :: RouterH
    , logoUrl :: Text
    , pages :: [Page]
    }

sidebar :: SidebarProps -> Element
sidebar (SidebarProps {..}) = div_ [style_ sidebarOuterStyle]
    [ div_ [style_ sidebarInnerStyle]
        [ div_ [style_ sidebarInnerTopStyle]
            [ link $ LinkProps
                { routerH, href = logoUrl, element =
                    a_ [style_ sidebarLogoContainerStyle, href_ logoUrl]
                        [ h1_ [style_ sidebarLogoStyle]
                            [ img_ [style_ sidebarLogoImageStyle, src_ ("https://interactivethings.github.io/catalog/docs/assets/catalog_logo.svg" :: Text)]
                            ]
                        ]
                }

            , ul_ [style_ ulStyle] $ map toMenuItem pages
            ]
        , div_ [style_ sidebarInnerBottomStyle]
            [ str_ "Powered by "
            , a_ [style_ sidebarInnerBottomLinkStyle, href_ ("https://github.io/nauva" :: Text)] [str_ "Nauva"]
            ]
        ]
    ]
  where
    toMenuItem :: Page -> Element
    toMenuItem (PLeaf (Leaf {..})) = li_ [ link (LinkProps { routerH = routerH, href = leafHref, element = a_ [style_ menuItemStyle, href_ leafHref] [ str_ leafTitle ] }) ]
    toMenuItem (PDirectory (Directory {..})) = div_
        [ link $ LinkProps { routerH = routerH, href = href, element = a_ [style_ menuItemStyle, href_ href] [ str_ directoryTitle ] }
        , ul_ [style_ ulStyle1] $ map toMenuItem1 directoryChildren
        ]
      where
        href = case directoryChildren of
            (Leaf {..}:_) -> leafHref
            _             -> ""

    toMenuItem1 :: Leaf -> Element
    toMenuItem1 (Leaf {..}) = li_ [ link (LinkProps { routerH = routerH, href = leafHref, element = a_ [style_ menuItemStyle1, href_ leafHref] [ str_ leafTitle ] }) ]

    sidebarOuterStyle = mkStyle $ do
        background "rgb(255, 255, 255)"
        color "rgb(255, 255, 255)"
        overflowY auto
        position fixed
        height "100vh"
        width "250px"
        top "0px"
        borderRight "1px solid rgb(235, 235, 235)"
        transform "translateX(-251px)"
        transition "transform 0.25s ease-in-out"

        media "(min-width: 1000px)" $ do
            transform "translateX(0px)"
            transition none

    sidebarInnerStyle = mkStyle $ do
        background "rgb(255, 255, 255)"
        minHeight "100vh"
        display flex
        flexDirection column

    sidebarInnerTopStyle = mkStyle $ do
        flex "1 1 0%"

    sidebarInnerBottomStyle = mkStyle $ do
        fontStyle "normal"
        fontWeight "400"
        color "rgb(214, 214, 214)"
        fontFamily "Roboto, sans-serif"
        fontSize "13.3333px"
        lineHeight "1.44"
        padding "20px"
        cssTerm "text-rendering" "optimizeLegibility"
        cssTerm "-webkit-font-smoothing" "antialiased"


    sidebarInnerBottomLinkStyle = mkStyle $ do
        color "rgb(214, 214, 214)"

    sidebarLogoContainerStyle = mkStyle $ do
        display block
        textDecoration none

    sidebarLogoStyle = mkStyle $ do
        fontStyle normal
        fontWeight "700"
        color "rgb(0, 59, 92)"
        fontFamily "Roboto, sans-serif"
        fontSize "19.2px"
        lineHeight "1.2"
        margin "0px"
        padding "21px 38px"
        height "200px"
        display flex
        alignItems flexEnd

    sidebarLogoImageStyle = mkStyle $ do
        maxWidth "100%"
        maxHeight "calc(100% - 39.8131px)"
        marginBottom "39.8131px"

    ulStyle = mkStyle $ do
        borderBottom "1px solid rgb(235, 235, 235)"
        listStyle none
        margin "0px"
        padding "0px"

    ulStyle1 = mkStyle $ do
        listStyle none
        margin "0px"
        padding "0px"

    menuItemStyle = mkStyle $ do
        fontStyle "normal"
        fontWeight "400"
        color "rgb(0, 59, 92)"
        fontFamily "Roboto, sans-serif"
        fontSize "16px"
        lineHeight "1.44"
        borderTop "1px solid rgb(235, 235, 235)"
        cursor "pointer"
        display "block"
        margin "0px"
        padding "16px 40px"
        textDecoration "none"

        onHover $ do
            color "rgb(255, 85, 85)"

    menuItemStyle1 = mkStyle $ do
        fontStyle "normal"
        fontWeight "400"
        color "rgb(0, 59, 92)"
        fontFamily "Roboto, sans-serif"
        fontSize "16px"
        lineHeight "1.44"
        cursor "pointer"
        display "block"
        margin "0px"
        padding "0px 24px 16px 60px"
        textDecoration "none"

        onHover $ do
            color "rgb(255, 85, 85)"
