{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Nauva.Product.Template.Catalog (catalogApp) where


import           Nauva.App

import           Nauva.Internal.Types

import           Nauva.Catalog
import           Nauva.Catalog.Types
import           Nauva.Catalog.TH
import           Nauva.View



catalogApp :: App
catalogApp = App
    { rootElement = \appH -> catalog $ CatalogProps (headH appH) (routerH appH) catalogPages
    }


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
