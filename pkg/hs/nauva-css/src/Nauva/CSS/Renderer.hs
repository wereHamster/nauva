{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}

module Nauva.CSS.Renderer
    ( renderCSSRule
    , cssRuleClass
    ) where


import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Monoid
import           Data.List

import           Nauva.CSS.Types



renderCSSDeclarations :: CSSStyleDeclaration -> Text
renderCSSDeclarations = mconcat . intersperse ";" . map renderDeclaration
  where
    renderDeclaration (k, CSSValue v) = "\n    " <> k <> ": " <> v

cssRuleClass :: Text -> Hash -> Text
cssRuleClass name hash = (if T.null name then "s-" else (name <> "-")) <> unHash hash

cssRuleSelector :: Text -> Hash -> [Suffix] -> Text
cssRuleSelector name hash suffixes = "." <> cssRuleClass name hash <> mconcat (map unSuffix suffixes)

wrapInConditions :: [Condition] -> Text -> Text
wrapInConditions [] t = t
wrapInConditions (CMedia x:xs) t = "@media " <> x <> " {" <> wrapInConditions xs t <> "\n}"
wrapInConditions (CSupports x:xs) t = "@supports " <> x <> " {" <> wrapInConditions xs t <> "\n}"

renderCSSRule :: CSSRule -> Text
renderCSSRule (CSSStyleRule name hash conditions suffixes styleDeclaration) = wrapInConditions conditions $ mconcat
    [ cssRuleSelector name hash suffixes <> " {"
    , renderCSSDeclarations styleDeclaration
    , "\n}"
    ]
renderCSSRule (CSSFontFaceRule _hash styleDeclaration) = mconcat
    [ "@font-face {"
    , renderCSSDeclarations styleDeclaration
    , "\n}"
    ]
