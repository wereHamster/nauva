{-# LANGUAGE OverloadedStrings #-}

module Main (main) where


import  Nauva.Client
import  App.Varna.Shared



main :: IO ()
main = do
    putStrLn "Native App"
    runClient $ Config
        { cElement = \_ -> rootElement 1
        }
