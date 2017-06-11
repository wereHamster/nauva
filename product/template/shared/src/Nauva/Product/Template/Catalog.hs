{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Nauva.Product.Template.Catalog (catalogApp) where


import           Nauva.Catalog
import           Nauva.Catalog.TH



catalogApp :: App
catalogApp = App
    { rootElement = catalog . CatalogProps catalogPages
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
