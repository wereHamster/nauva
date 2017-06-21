module Main (main) where


import           Nauva.Client
import           Nauva.Product.Varna.Catalog



main :: IO ()
main = do
    putStrLn "Varna Catalog"
    runClient catalogApp