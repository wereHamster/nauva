{-# LANGUAGE OverloadedStrings #-}

module Main (main) where


import           Nauva.App (App(App))
import           Nauva.Server
import           Nauva.Product.Playground.Shared



main :: IO ()
main = devServer $ App (\_ -> rootElement 1)
