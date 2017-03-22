{-# LANGUAGE OverloadedStrings #-}

module Main (main) where


import qualified Text.Blaze.Html5            as H

import           Nauva.Server
import           Nauva.Product.Playground.Shared



main :: IO ()
main = do
    runServer $ Config 8000 (\_ -> rootElement 1) Nothing $ do
        H.style $ mconcat
            [ "*, *:before, *:after { box-sizing: inherit; }"
            , "html { box-sizing: border-box; }"
            , "body { margin: 0; }"
            ]
