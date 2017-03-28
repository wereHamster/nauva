{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Nauva.Product.Template.Catalog (catalogPages) where


import           Nauva.Internal.Types

import           Nauva.Catalog.Types
import           Nauva.Catalog.TH



catalogPages :: [Page]
catalogPages =
    [ PLeaf Leaf
        { leafHref = "/"
        , leafTitle = "Introduction"
        , leafElement = introductionPage
        }
    ]


introductionPage :: Element
introductionPage = [nauvaCatalogPage|
# Welcome to the Template catalog
|]
