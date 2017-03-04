module Nauva.CSS.Terms.Generator (main) where

import Data.List (sort, nub)
import Data.Char (toUpper)


sanitize :: String -> String
sanitize = removeDash
  where
    removeDash ('-' : x : xs) = toUpper x : removeDash xs
    removeDash (x : xs)       = x : removeDash xs
    removeDash []             = []


exportList :: [String] -> String
exportList []            = error "exportList without functions."
exportList (f:functions) = unlines $
    [ "module Nauva.CSS.Terms"
    , "    ( " ++ f
    ] ++
    map ("    , " ++) functions ++
    [ "    ) where"]


makeTerm :: String -> String
makeTerm tag = unlines
    [ function ++ " :: CSSTerm a => a"
    , function ++ " = cssTerm \"" ++ tag ++ "\""
    , "{-# INLINE " ++ function ++ " #-}"
    ]
  where
    function = sanitize tag

-- Terms which can appear both as properties and values.
properties :: [String]
properties =
    [ "align-items"
    , "background"
    , "background-color"
    , "border"
    , "border-bottom"
    , "border-left"
    , "border-radius"
    , "border-right"
    , "border-top"
    , "bottom"
    , "box-sizing"
    , "color"
    , "content"
    , "cursor"
    , "display"
    , "flex-basis"
    , "flex-direction"
    , "flex-end"
    , "flex-flow"
    , "flex-shrink"
    , "flex-wrap"
    , "flex"
    , "font-family"
    , "font-size"
    , "font-style"
    , "font-variant"
    , "font-weight"
    , "height"
    , "hyphens"
    , "justify-content"
    , "left"
    , "line-height"
    , "list-style"
    , "margin-bottom"
    , "margin-left"
    , "margin-right"
    , "margin-top"
    , "margin"
    , "max-height"
    , "max-width"
    , "min-height"
    , "opacity"
    , "outline"
    , "overflow"
    , "overflowY"
    , "padding"
    , "padding-bottom"
    , "padding-left"
    , "padding-right"
    , "padding-top"
    , "position"
    , "quotes"
    , "src"
    , "text-align"
    , "text-decoration"
    , "text-indent"
    , "text-rendering"
    , "text-transform"
    , "top"
    , "transform"
    , "transition"
    , "user-select"
    , "white-space"
    , "width"
    ]

-- Terms which only appear as values.
values :: [String]
values =
    [ "absolute"
    , "auto"
    , "block"
    , "center"
    , "column"
    , "fixed"
    , "inline-block"
    , "none"
    , "normal"
    , "pointer"
    , "relative"
    , "right"
    , "row"
    , "uppercase"
    , "wrap"
    ]

terms :: [String]
terms = nub $ sort $ properties ++ values

main :: IO ()
main = putStr $ removeTrailingNewlines $ unlines
    [ "{-# LANGUAGE OverloadedStrings #-}"
    , ""
    , exportList (map sanitize terms)
    , ""
    , "import Nauva.CSS.Types"
    , ""
    , ""
    , unlines $ map makeTerm terms
    ]

  where
    removeTrailingNewlines = reverse . drop 2 . reverse
