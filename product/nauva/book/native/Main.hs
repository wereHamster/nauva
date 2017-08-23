module Main (main) where

import Nauva.Client
import Nauva.Product.Nauva.Book.App


main :: IO ()
main = runClient bookApp
