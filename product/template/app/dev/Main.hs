{-# LANGUAGE OverloadedStrings #-}

module Main (main) where


import           Nauva.Server
import           Nauva.View

import           Nauva.Service.Head
import           Nauva.Service.Router



main :: IO ()
main = runServer $ Config
    { cElement = app
    , cHead = mempty
    }


app :: HeadH -> RouterH -> Element
app headH routerH = div_ [style_ style]
    [ header
    , intro
    ]
  where
    style = mkStyle $ do
        textAlign center


header :: Element
header = div_ [style_ style]
    [ h1_ [str_ "Welcome to Nauva"]
    ]
  where
    style = mkStyle $ do
        backgroundColor "#222"
        height "150px"
        padding "20px"
        color "white"

intro :: Element
intro = p_ [style_ style] [str_ "To get started, edit ??? and save to reload."]
  where
    style = mkStyle $ do
        fontSize "large"
