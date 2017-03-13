module Main (main) where


import  Nauva.Client
import  Nauva.Product.Varna.Shared



main :: IO ()
main = do
    putStrLn "Native App"
    runClient $ Config
        { cElement = \_ -> root
        }
