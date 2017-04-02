{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE NamedFieldPuns        #-}

module Nauva.Catalog
    ( CatalogProps(..)
    , catalog
    ) where


import           Data.Text  (Text)
import qualified Data.Aeson as A
import           Data.Maybe

import           Control.Concurrent.STM

import           Nauva.Internal.Types (Signal(..), Element, Component(..), createComponent, emptyHooks)
import           Nauva.View

import           Nauva.Service.Router

import           Nauva.Catalog.Shell
import           Nauva.Catalog.Types



-------------------------------------------------------------------------------
-- The 'catalog' element is meant to be used as the root element of the
-- application. It renders the sidebar (navigation) and page content depending
-- on the current location.

data CatalogProps = CatalogProps
    { p_routerH :: !RouterH
    , p_pages :: ![Page]
    }

catalog :: CatalogProps -> Element
catalog = component_ catalogComponent



-------------------------------------------------------------------------------

data State = State
    { path :: !Text
    }

catalogComponent :: Component CatalogProps () State ()
catalogComponent = createComponent $ \componentId -> Component
    { componentId = componentId
    , componentDisplayName = "Catalog"

    , initialComponentState = \props -> do
        loc <- readTVar $ fst $ hLocation $ p_routerH (props :: CatalogProps)
        pure
            ( State (locPathname loc)
            , [ Signal (snd $ hLocation $ p_routerH (props :: CatalogProps)) (\(Location p) s -> (s { path = p }, [])) ]
            )

    , componentEventListeners = \_ -> []
    , componentHooks = emptyHooks
    , processLifecycleEvent = \() _ s -> (s, [])
    , receiveProps = \props s -> pure (s, [Signal (snd $ hLocation $ p_routerH (props :: CatalogProps)) (\(Location p) s' -> (s' { path = p }, []))], [])
    , update = update
    , renderComponent = render
    , componentSnapshot = \_ -> A.object []
    , restoreComponent = \_ s -> Right (s, [])
    }
  where
    update () _ s = (s, [])

    render props (State {..}) = div_ [style_ rootStyle]
        [ div_ [style_ mainStyle]
            [ header (HeaderProps { section, title })
            , div_ [style_ pageStyle] [page]
            ]

        , sidebar $ SidebarProps
            { p_routerH = p_routerH (props :: CatalogProps)
            , p_logoUrl
            , p_pages   = p_pages (props :: CatalogProps)
            }
        ]
      where
        p_logoUrl = "/"

        flattenPage :: Page -> [(Text, Leaf)]
        flattenPage (PLeaf leaf@(Leaf {..})) = [(leafHref, leaf)]
        flattenPage (PDirectory (Directory {..})) = map (\x -> (leafHref x, x)) directoryChildren

        flattenedPages = concat $ map flattenPage $ p_pages (props :: CatalogProps)

        page = case lookup path flattenedPages of
            Nothing   -> div_ [style_ pageInnerStyle] [str_ "page not found"]
            Just leaf -> leafElement leaf

        section :: Text
        section = findSection Nothing $ p_pages (props :: CatalogProps)
          where
            findSection mbTitle []                               = fromMaybe "Catalog" mbTitle
            findSection _       (PDirectory (Directory {..}):xs) = findSection (Just directoryTitle) xs
            findSection mbTitle (PLeaf (Leaf {..}):xs)           = if leafHref == path
                then fromMaybe "Catalog" mbTitle
                else findSection mbTitle xs

        title :: Text
        title = case lookup path flattenedPages of
            Nothing   -> "Unknown Page"
            Just leaf -> leafTitle leaf

        rootStyle = mkStyle $ do
            position relative
            background "rgb(249, 249, 249)"
            margin "0px"
            padding "0px"
            width "100%"
            height "100%"

        mainStyle = mkStyle $ do
            display flex
            flexDirection column
            minHeight (vh 100)
            position relative

            -- media "(min-width: 1000px)" $ do
            marginLeft (px 251)

        pageStyle = mkStyle $ do
            flex "1 1 0%"

        pageInnerStyle = mkStyle $ do
            margin "0 30px 0 40px"
            maxWidth "64em"
            display flex
            flexFlow row wrap
            padding "48px 0px"
