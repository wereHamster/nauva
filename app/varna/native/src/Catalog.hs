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

import           App.Varna.Catalog (catalogPages)



main :: IO ()
main = do
    putStrLn "Varna Catalog"
    runClient $ Config
        { cElement = \routerH -> catalog $ CatalogProps routerH catalogPages
        }
