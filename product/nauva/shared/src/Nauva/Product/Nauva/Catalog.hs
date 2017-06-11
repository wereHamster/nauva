{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

module Nauva.Product.Nauva.Catalog (catalogApp) where


import           Nauva.App

import           Nauva.Internal.Types

import           Nauva.Catalog
import           Nauva.Catalog.Types
import           Nauva.Catalog.TH

import           Nauva.Product.Nauva.Element.Terminal as Terminal
import           Nauva.Product.Nauva.Element.Message  as Message



catalogApp :: App
catalogApp = App
    { rootElement = catalog . CatalogProps catalogPages
    }


catalogPages :: [Page]
catalogPages =
    [ PLeaf $ Leaf
        { leafHref = "/"
        , leafTitle = "Introduction"
        , leafElement = introductionPage
        }
    , PDirectory $ Directory
        { directoryTitle = "Theme"
        , directoryChildren =
            [ Leaf
                { leafHref = "/theme/colors"
                , leafTitle = "Colors"
                , leafElement = Message.catalogPage
                }
            , Leaf
                { leafHref = "/theme/typefaces"
                , leafTitle = "Typefaces"
                , leafElement = Message.catalogPage
                }
            ]
        }
    , PDirectory $ Directory
        { directoryTitle = "Elements"
        , directoryChildren =
            [ Leaf
                { leafHref = "/elements/terminal"
                , leafTitle = "Terminal"
                , leafElement = Terminal.catalogPage
                }
            , Leaf
                { leafHref = "/elements/message"
                , leafTitle = "Message"
                , leafElement = Message.catalogPage
                }
            ]
        }
    ]


introductionPage :: Element
introductionPage = [nauvaCatalogPage|
# Welcome to the Nauva catalog
|]
