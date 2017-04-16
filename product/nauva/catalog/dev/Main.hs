module Main (main) where

import           Nauva.Server
import           Nauva.Product.Nauva.Catalog (catalogApp)


main :: IO ()
main = devServer catalogApp