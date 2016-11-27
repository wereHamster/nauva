module Main (main) where

import Nauva.Client
import Nauva.Playground.App

main :: IO ()
main = do
    putStrLn "Native App"
    runClient (Config (rootElement 1))
