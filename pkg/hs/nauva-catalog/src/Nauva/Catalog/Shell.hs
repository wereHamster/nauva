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


import           Data.Text          (Text)

import           Nauva.Internal.Types
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
headerThunk = createThunk $ \thunkId -> Thunk
    { thunkId = thunkId
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
        backgroundColor "#373f52"
        padding (px 20) (px 30)

        display flex
        flexDirection column
        justifyContent "flex-end"

    headerSectionStyle = mkStyle $ do
        fontFamily "Roboto, sans-serif"
        fontStyle normal
        fontWeight "300"
        color "white"
        fontSize (px 19.2)
        lineHeight "1.2"
        opacity "0.6"
        margin "0"

    headerTitleStyle = mkStyle $ do
        fontFamily "Roboto, sans-serif"
        fontStyle normal
        fontWeight "300"
        color "white"
        fontSize (px 33.1776)
        lineHeight "1.2"
        margin "0"



-------------------------------------------------------------------------------
-- sidebar

data SidebarProps = SidebarProps
    { p_routerH :: RouterH
    , p_logoUrl :: Text
    , p_pages :: [Page]
    }

sidebar :: SidebarProps -> Element
sidebar (SidebarProps {..}) = div_ [style_ sidebarOuterStyle]
    [ div_ [style_ sidebarInnerStyle]
        [ div_ [style_ sidebarInnerTopStyle]
            [ link $ LinkProps
                { p_routerH, p_href = p_logoUrl, p_element =
                    a_ [style_ sidebarLogoContainerStyle, href_ p_logoUrl]
                        [ h1_ [style_ sidebarLogoStyle]
                            [ -- img_ [style_ sidebarLogoImageStyle, src_ ("https://interactivethings.github.io/catalog/docs/assets/catalog_logo.svg" :: Text)]
                            ]
                        ]
                }

            , ul_ [style_ ulStyle] $ map toMenuItem p_pages
            ]
        , div_ [style_ sidebarInnerBottomStyle]
            [ str_ "Powered by "
            , a_ [style_ sidebarInnerBottomLinkStyle, href_ ("https://github.io/nauva" :: Text)] [str_ "Nauva"]
            ]
        ]
    ]
  where
    toMenuItem :: Page -> Element
    toMenuItem (PLeaf (Leaf {..})) = li_ [ link (LinkProps { p_routerH = p_routerH, p_href = leafHref, p_element = a_ [style_ menuItemStyle, href_ leafHref] [ str_ leafTitle ] }) ]
    toMenuItem (PDirectory (Directory {..})) = div_
        [ link $ LinkProps { p_routerH = p_routerH, p_href = p_href, p_element = a_ [style_ menuItemStyle, href_ p_href] [ str_ directoryTitle ] }
        , ul_ [style_ ulStyle1] $ map toMenuItem1 directoryChildren
        ]
      where
        p_href = case directoryChildren of
            (Leaf {..}:_) -> leafHref
            _             -> ""

    toMenuItem1 :: Leaf -> Element
    toMenuItem1 (Leaf {..}) = li_ [ link (LinkProps { p_routerH = p_routerH, p_href = leafHref, p_element = a_ [style_ menuItemStyle1, href_ leafHref] [ str_ leafTitle ] }) ]

    sidebarOuterStyle = mkStyle $ do
        background "#373f52"
        color "#d5dae6"
        overflowY auto
        position fixed
        height "100vh"
        width "250px"
        top "0px"
        -- transform "translateX(-251px)"
        -- transition "transform 0.25s ease-in-out"

        -- media "(min-width: 1000px)" $ do
        transform "translateX(0px)"
        transition none

    sidebarInnerStyle = mkStyle $ do
        background "#373f52"
        minHeight "100vh"
        display flex
        flexDirection column

    sidebarInnerTopStyle = mkStyle $ do
        flex "1 1 0%"

    sidebarInnerBottomStyle = mkStyle $ do
        fontStyle "normal"
        fontWeight "400"
        color "#d5dae6"
        fontFamily "Roboto, sans-serif"
        fontSize "13.3333px"
        lineHeight "1.44"
        padding "20px"
        textRendering "optimizeLegibility"
        cssTerm "-webkit-font-smoothing" "antialiased"


    sidebarInnerBottomLinkStyle = mkStyle $ do
        color "#d5dae6"

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
        margin "0"
        padding "21px 38px"
        height "200px"
        display flex
        alignItems flexEnd

    -- sidebarLogoImageStyle = mkStyle $ do
    --     maxWidth "100%"
    --     maxHeight "calc(100% - 39.8131px)"
    --     marginBottom "39.8131px"

    ulStyle = mkStyle $ do
        listStyle none
        margin "0"
        padding "30px 0 0 30px"

    ulStyle1 = mkStyle $ do
        listStyle none
        margin "0"
        padding "0"

    menuItemStyle = mkStyle $ do
        fontStyle "normal"
        fontWeight "300"
        color "#d5dae6"
        fontFamily "Roboto, sans-serif"
        fontSize "16px"
        lineHeight "27px"
        cursor "pointer"
        display "block"
        margin "0"
        padding "16px" "0" "0"
        textDecoration "none"
        transition "color .16s"

        onHover $ do
            color "white"

    menuItemStyle1 = mkStyle $ do
        fontStyle "normal"
        fontWeight "300"
        color "#d5dae6"
        fontFamily "Roboto, sans-serif"
        fontSize "16px"
        lineHeight "27px"
        cursor "pointer"
        display "block"
        margin "0"
        padding "0 24px 0 20px"
        textDecoration "none"
        transition "color .16s"

        onHover $ do
            color "white"
