{-# LANGUAGE OverloadedStrings #-}

module Main (main) where


import           Nauva.Server
import           Nauva.View

import           Nauva.Service.Head
import           Nauva.Service.Router



main :: IO ()
main = runServer $ Config
    { cElement = app
    }


app :: HeadH -> RouterH -> Element
app headH routerH = constHead headH headElements $ div_ [style_ style]
    [ header
    , intro
    ]
  where
    headElements =
        [ style_ [str_ "*,*::before,*::after{box-sizing:inherit}body{margin:0;box-sizing:border-box;font-family:-apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, Helvetica, Arial, sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\"}"]
        ]

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
