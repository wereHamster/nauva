module Nauva.CSS.Terms.Generator (main) where

import Data.List (sort, nub)
import Data.Char (toUpper)


sanitize :: String -> String
sanitize str = removeDash str
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
    , "background-color"
    , "border"
    , "color"
    , "display"
    , "flex-direction"
    , "flex"
    , "font-family"
    , "font-size"
    , "height"
    , "line-height"
    , "margin-left"
    , "margin-right"
    , "margin"
    , "outline"
    , "src"
    , "text-align"
    , "width"
    ]

-- Terms which only appear as values.
values :: [String]
values =
    [ "block"
    , "center"
    , "column"
    , "none"
    , "right"
    , "row"
    ]

terms :: [String]
terms = nub $ sort $ properties ++ values

main :: IO ()
main = do
    putStr $ removeTrailingNewlines $ unlines
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
