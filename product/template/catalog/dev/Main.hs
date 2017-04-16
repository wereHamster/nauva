module Main (main) where

import Nauva.Server
import Nauva.Product.Template.Catalog (catalogApp)


main :: IO ()
main = devServer catalogApp
