{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Nauva.Product.Template.App (app) where


import           Nauva.App
import           Nauva.View

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax



app :: App
app = App
    { rootElement = \appH -> constHead (headH appH) headElements $ div_ [style_ style]
        [ header
        , intro
        ]
    }

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
intro = p_ [style_ style]
    [ str_ "To get started, edit "
    , code_ [str_ thisFilePath]
    , str_ " and save to reload."
    ]
  where
    style = mkStyle $ do
        fontSize "large"

    -- The path to this file. Here we use a bit of TemplateHaskell magic
    -- so that we can show the exact path the user has to edit to get
    -- started
    thisFilePath = $(lift =<< loc_filename <$> location)
