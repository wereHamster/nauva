{-# LANGUAGE OverloadedStrings #-}

module Main (main) where


import           Nauva.Client
import           Nauva.Product.Playground.Shared



main :: IO ()
main = do
    putStrLn "playground/app/native"
    runClient playgroundApp
