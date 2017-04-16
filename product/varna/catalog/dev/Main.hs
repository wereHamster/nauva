module Main (main) where

import           Nauva.Server
import           Nauva.Product.Varna.Catalog (catalogApp)


main :: IO ()
main = devServer catalogApp
