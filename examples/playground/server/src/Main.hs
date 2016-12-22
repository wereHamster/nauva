{-# LANGUAGE OverloadedStrings #-}

module Main (main) where


import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

import           Nauva.Server
import           Nauva.Playground.App



main :: IO ()
main = do
    runServer $ Config 8000 (rootElement 1) Nothing $ do
        H.style $ mconcat
            [ "*, *:before, *:after { box-sizing: inherit; }"
            , "html { box-sizing: border-box; }"
            , "body { margin: 0; }"
            ]
