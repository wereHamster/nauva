{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

module Main (main) where


import           Nauva.App
import           Nauva.Client
import           Nauva.Catalog
import           Nauva.Product.Template.Catalog (catalogApp)



main :: IO ()
main = do
    putStrLn "Catalog App"
    runClient catalogApp
