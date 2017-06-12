module Main (main) where

import Nauva.Server
import Nauva.Product.Nauva.Book.App


main :: IO ()
main = devServer bookApp
