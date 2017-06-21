module Main (main) where

import           Nauva.App
import           Nauva.Server
import           Nauva.Product.Varna.Shared


main :: IO ()
main = devServer $ App (root . headH)
