{-# LANGUAGE OverloadedStrings #-}

module Main (main) where


import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

import           Nauva.Server

import           App.Varna.Shared



main :: IO ()
main = do
    runServer $ Config 8000 (\_ -> rootElement 1) Nothing $ do
        H.script H.! A.src "https://use.typekit.net/ubj7dti.js" $ ""
        H.script "try{Typekit.load({ async: true });}catch(e){}"

        H.style $ mconcat
            [ "*, *:before, *:after { box-sizing: inherit; }"
            , "html { box-sizing: border-box; }"
            , "body { margin: 0; }"
            ]
