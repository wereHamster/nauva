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

properties :: [String]
properties =
    [ "align-items"
    , "background-color"
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
    , "text-align"
    , "width"
    ]

values :: [String]
values =
    [ "block"
    , "center"
    , "column"
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
