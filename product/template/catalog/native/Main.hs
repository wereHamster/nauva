{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

module Main (main) where


import           Nauva.App
import           Nauva.Client
import           Nauva.Catalog
import           Nauva.Product.Template.Catalog (catalogApp)

import           Nauva.Service.Head
import           Nauva.Service.Router


main :: IO ()
main = do
    putStrLn "Catalog App"
    runClient catalogApp
