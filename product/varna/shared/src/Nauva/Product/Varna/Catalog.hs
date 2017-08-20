{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

module Nauva.Product.Varna.Catalog (catalogApp) where

import           Nauva.App
import           Nauva.Catalog
import           Nauva.Catalog.TH

import           Nauva.Product.Varna.Element.Card as Card



catalogApp :: App
catalogApp = App
    { rootElement = catalog . CatalogProps "Varna" catalogPages
    }

catalogPages :: [Page]
catalogPages =
    [ PLeaf Leaf
        { leafHref = "/"
        , leafTitle = "Introduction"
        , leafElement = introductionPage
        }
    , PDirectory Directory
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
