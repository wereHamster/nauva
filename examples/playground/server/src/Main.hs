module Main (main) where

import Nauva.Server
import Nauva.Playground.App

main :: IO ()
main = do
    runServer $ Config 8000 (rootElement 1)
