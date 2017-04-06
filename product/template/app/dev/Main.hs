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
app headH routerH = div_
    [ header
    ]

header :: Element
header = div_ [style_ rootStyle] [str_ "Template Application"]
  where
    rootStyle = mkStyle $ do
        height "calc(.4 * 100vh)"
        backgroundColor "#F1F1F1"
        display flex
        justifyContent center
        alignItems center
        fontSize (px 24)
