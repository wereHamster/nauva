{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

module Main (main) where


import           Nauva.Internal.Types
import           Nauva.Client
import           Nauva.Catalog
import           Nauva.Catalog.Types
import           Nauva.Catalog.TH
import           Nauva.Catalog.Shell
import           Nauva.Catalog.Elements

import           App.Varna.Element.Card as Card



main :: IO ()
main = do
    putStrLn "Varna Catalog"
    runClient $ Config
        { cElement = \routerH -> catalog $ CatalogProps routerH catalogPages
        }

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

