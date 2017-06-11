module Main (main) where

import Nauva.Server
import Nauva.Product.Template.App


main :: IO ()
main = devServer app
