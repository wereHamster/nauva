module Main (main) where

import Nauva.Client
import Nauva.Product.Template.Catalog (catalogApp)


main :: IO ()
main = do
    putStrLn "Catalog App"
    runClient catalogApp
