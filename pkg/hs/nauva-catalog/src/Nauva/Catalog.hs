{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE NamedFieldPuns        #-}

module Nauva.Catalog
    ( CatalogProps(..)
    , catalog

    , module Nauva.App
    , module Nauva.View
    , module Nauva.Catalog.Types
    ) where


import           Data.Text  (Text)
import qualified Data.Aeson as A
import           Data.Maybe

import           Control.Concurrent.STM

import           Nauva.App
import           Nauva.Internal.Types (Signal(..), Element, Component(..), createComponent, emptyHooks)
import           Nauva.View

import           Nauva.Catalog.Shell
import           Nauva.Catalog.Types



-------------------------------------------------------------------------------
-- The 'catalog' element is meant to be used as the root element of the
-- application. It renders the sidebar (navigation) and page content depending
-- on the current location.

data CatalogProps = CatalogProps
    { p_pages :: ![Page]
    , p_appH :: !AppH
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
        loc <- readTVar $ fst $ hLocation $ routerH $ p_appH (props :: CatalogProps)
        pure
            ( State (locPathname loc)
            , [ Signal (snd $ hLocation $ routerH $ p_appH (props :: CatalogProps)) (\(Location p) props' s -> (s { path = p }, [updateHead props' p])) ]
            , [ updateHead props (locPathname loc) ]
            )

    , componentEventListeners = const []
    , componentHooks = emptyHooks
    , processLifecycleEvent = \() _ s -> (s, [])
    , receiveProps = \props s -> pure (s, [Signal (snd $ hLocation $ routerH $ p_appH (props :: CatalogProps)) (\(Location p) props' s' -> (s' { path = p }, [updateHead props' p]))], [])
    , update = update
    , renderComponent = render
    , componentSnapshot = \_ -> A.object []
    , restoreComponent = \_ s -> Right (s, [])
    }
  where
    updateHead :: CatalogProps -> Text -> IO (Maybe ())
    updateHead props path = do
        hReplace (headH $ p_appH props)
            [ style_ [str_ "*,*::before,*::after{box-sizing:inherit}body{margin:0;box-sizing:border-box}"]
            , title_ [str_ $ title (p_pages (props :: CatalogProps)) path]

            , link_ [rel_ ("stylesheet" :: Text), type_ ("text/css" :: Text), href_ ("https://fonts.googleapis.com/css?family=Roboto:400,700,400italic" :: Text)]
            , link_ [rel_ ("stylesheet" :: Text), type_ ("text/css" :: Text), href_ ("https://fonts.googleapis.com/css?family=Source+Code+Pro:400,700" :: Text)]
            , link_ [rel_ ("stylesheet" :: Text), type_ ("text/css" :: Text), href_ ("https://fonts.googleapis.com/css?family=Open+Sans:300,300i,400,400i,600,600i,700" :: Text)]
            ]
        pure Nothing

    update () props s =
        ( s
        , [ updateHead props (path s) ]
        )

    title :: [Page] -> Text -> Text
    title pages p = case lookup p (flattenedPages pages) of
        Nothing   -> "Unknown Page"
        Just leaf -> leafTitle leaf

    flattenPage :: Page -> [(Text, Leaf)]
    flattenPage (PLeaf leaf@(Leaf {..})) = [(leafHref, leaf)]
    flattenPage (PDirectory (Directory {..})) = map (\x -> (leafHref x, x)) directoryChildren

    flattenedPages pages = concat $ map flattenPage pages

    render props (State {..}) = div_ [style_ rootStyle]
        [ div_ [style_ mainStyle]
            [ header (HeaderProps { section, title = title (p_pages (props :: CatalogProps)) path })
            , div_ [style_ pageStyle] [page]
            ]

        , sidebar $ SidebarProps
            { p_routerH = routerH $ p_appH (props :: CatalogProps)
            , p_logoUrl
            , p_pages = p_pages (props :: CatalogProps)
            }
        ]
      where
        p_logoUrl = "/"

        page = case lookup path (flattenedPages $ p_pages (props :: CatalogProps)) of
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
