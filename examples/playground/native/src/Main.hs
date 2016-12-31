{-# LANGUAGE OverloadedStrings #-}

module Main (main) where


import qualified Text.Blaze.Html            as H
import qualified Text.Blaze.Html.Attributes as A

import           Nauva.Client
import           Nauva.Playground.App



main :: IO ()
main = do
    putStrLn "Native App"
    runClient (Config (rootElement 1)) Nothing $ do
        H.style $ mconcat
            [ "*, *:before, *:after { box-sizing: inherit; }"
            , "html { box-sizing: border-box; }"
            , "body { margin: 0; }"
            ]
