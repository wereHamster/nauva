{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

module Nauva.Product.Varna.Catalog (catalogPages) where


import           Nauva.Internal.Types

import           Nauva.Catalog.Types
import           Nauva.Catalog.TH
import           Nauva.Catalog.Elements

import           Nauva.Product.Varna.Element.Card as Card



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
