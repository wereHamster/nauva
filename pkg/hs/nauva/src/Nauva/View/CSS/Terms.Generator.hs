module Nauva.View.CSS.Terms.Generator (main) where

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
    [ "module Nauva.View.CSS.Terms"
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
    [ "display"
    , "height"
    , "flex"
    , "flex-direction"
    , "width"
    ]

values :: [String]
values =
    [ "block"
    , "column"
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
        , "import Nauva.View.CSS.Internal"
        , ""
        , ""
        , unlines $ map makeTerm terms
        ]

  where
    removeTrailingNewlines = reverse . drop 2 . reverse
