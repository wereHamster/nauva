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
        backgroundColor "#444"
        color "white"
        display flex
        justifyContent center
        alignItems center
        fontSize (px 24)
        fontFamily "-apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol'"
        fontSize "24px"













{-

width "100vw"
position fixed
top "0"
left "0"
fontFamily "-apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol'"
color "white"

-}
