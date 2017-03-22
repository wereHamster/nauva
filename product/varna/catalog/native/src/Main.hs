{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

module Main (main) where


import           Nauva.Client
import           Nauva.Catalog
import           Nauva.Product.Varna.Catalog (catalogPages)



main :: IO ()
main = do
    putStrLn "Varna Catalog"
    runClient $ Config
        { cElement = \routerH -> catalog $ CatalogProps routerH catalogPages
        }
