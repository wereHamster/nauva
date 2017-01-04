module Nauva.View.Terms.Generator (main) where

import Data.List (sort, nub)
import Data.Char (toUpper)


sanitize :: String -> String
sanitize str = removeDash str ++ "_"
  where
    removeDash ('-' : x : xs) = toUpper x : removeDash xs
    removeDash (x : xs)       = x : removeDash xs
    removeDash []             = []


exportList :: [String] -> String
exportList []            = error "exportList without functions."
exportList (f:functions) = unlines $
    [ "module Nauva.View.Terms"
    , "    ( " ++ f
    ] ++
    map ("    , " ++) functions ++
    [ "    ) where"]


makeVoidTerm :: String -> String
makeVoidTerm tag = unlines
    [ function ++ " :: [Attribute] -> Element"
    , function ++ " = with (ENode \"" ++ tag ++ "\" [] [])"
    , "{-# INLINE " ++ function ++ " #-}"
    ]
  where
    function = sanitize tag

makeTerm :: String -> String
makeTerm tag = unlines
    [ function ++ " :: Term arg res => arg -> res"
    , function ++ " = term \"" ++ tag ++ "\""
    , "{-# INLINE " ++ function ++ " #-}"
    ]
  where
    function = sanitize tag

makeAttributeTerm :: String -> String
makeAttributeTerm tag = unlines
    [ function ++ " :: Term arg res => arg -> res"
    , function ++ " = term \"" ++ tag ++ "\""
    , "{-# INLINE " ++ function ++ " #-}"
    ]
  where
    function = sanitize tag


-- For distinction between void and normal elements, please refer to
-- https://www.w3.org/TR/html5/syntax.html#elements-0

voidElements :: [String]
voidElements =
    [ "area"
    , "base"
    , "br"
    , "col"
    , "embed"
    , "hr"
    , "img"
    , "input"
    , "keygen"
    , "link"
    , "meta"
    , "param"
    , "source"
    , "track"
    , "wbr"
    ]

normalElements :: [String]
normalElements =
    [ "a"
    , "blockquote"
    , "button"
    , "circle"
    , "code"
    , "div"
    , "h1"
    , "h2"
    , "h3"
    , "h4"
    , "h5"
    , "h6"
    , "i"
    , "li"
    , "p"
    , "pre"
    , "rect"
    , "section"
    , "span"
    , "strong"
    , "style"
    , "svg"
    , "ul"
    , "ol"
    , "value"
    ]

attributes :: [String]
attributes =
    [ "className"
    , "width"
    , "height"
    , "href"
    , "r"
    , "x"
    , "y"
    , "cx"
    , "cy"
    , "fill"
    , "ref"
    , "src"
    ]

terms :: [String]
terms = nub $ sort $ voidElements ++ normalElements ++ attributes

main :: IO ()
main = do
    putStr $ removeTrailingNewlines $ unlines
        [ "{-# LANGUAGE OverloadedStrings #-}"
        , "{-# LANGUAGE TypeFamilies      #-}"
        , ""
        , exportList (map sanitize terms)
        , ""
        , "import Nauva.Internal.Types"
        , "import Nauva.View.Types"
        , ""
        , ""
        , unlines $ map makeVoidTerm (nub $ sort $ voidElements)
        , ""
        , unlines $ map makeTerm (nub $ sort $ normalElements)
        , ""
        , unlines $ map makeAttributeTerm (nub $ sort $ attributes)
        ]

  where
    removeTrailingNewlines = reverse . drop 2 . reverse
