{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

module Main (main, catalogMain) where


import           Data.ByteString    (ByteString)
import qualified Data.ByteString    as BS
import           Data.Text          (Text)
import qualified Data.Text.Encoding as T
import           Text.Markdown      (def, MarkdownSettings(..))
import           Text.Markdown.Block
import           Text.Markdown.Inline
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Functor.Identity (runIdentity)

import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

import           Nauva.Server
import           Nauva.DOM
import           Nauva.Internal.Types
import           Nauva.Internal.Events
import           Nauva.NJS
import           Nauva.View

import           Nauva.Catalog
import           Nauva.Catalog.Types
import           Nauva.Catalog.TH
import           Nauva.Catalog.Shell
import           Nauva.Catalog.Elements

import           App.Varna.Shared

import           App.Varna.Element.Card as Card



main :: IO ()
main = do
    runServer $ Config 8000 (\_ -> rootElement 1) Nothing $ do
        H.script H.! A.src "https://use.typekit.net/ubj7dti.js" $ ""
        H.script "try{Typekit.load({ async: true });}catch(e){}"

        H.style $ mconcat
            [ "*, *:before, *:after { box-sizing: inherit; }"
            , "html { box-sizing: border-box; }"
            , "body { margin: 0; }"
            ]


catalogMain :: IO ()
catalogMain = do
    runServer $ Config 8000 (\routerH -> catalog $ CatalogProps routerH catalogPages) Nothing $ do
        H.link H.! A.rel "stylesheet" H.! A.type_ "text/css" H.! A.href "https://fonts.googleapis.com/css?family=Roboto:400,700,400italic"
        H.link H.! A.rel "stylesheet" H.! A.type_ "text/css" H.! A.href "https://fonts.googleapis.com/css?family=Source+Code+Pro:400,700"

        H.script H.! A.src "https://use.typekit.net/ubj7dti.js" $ ""
        H.script "try{Typekit.load({ async: true });}catch(e){}"

        H.style $ mconcat
            [ "*, *:before, *:after { box-sizing: inherit; }"
            , "html { box-sizing: border-box; }"
            , "body { margin: 0; }"
            ]


catalogPages :: [Page]
catalogPages =
    [ PLeaf $ Leaf
        { leafHref = "/"
        , leafTitle = "Introduction"
        , leafElement = introductionPage
        }
    , PDirectory $ Directory
        { directoryTitle = "Elements"
        , directoryChildren =
            [ Leaf
                { leafHref = "/elements/card"
                , leafTitle = "Card"
                , leafElement = Card.catalogPage
                }
            ]
        }
    ]

introductionPage :: Element
introductionPage = [nauvaCatalogPage|
# Welcome to the Varna catalog
|]
