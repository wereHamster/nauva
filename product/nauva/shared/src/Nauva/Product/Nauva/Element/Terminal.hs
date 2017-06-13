{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Nauva.Product.Nauva.Element.Terminal
    ( terminalEl
    , TerminalProps(..)

    , catalogPage
    ) where


import           Data.Text (Text)

import           Nauva.Internal.Types
import           Nauva.View

import           Nauva.Catalog.TH (nauvaCatalogPage)
import           Nauva.Catalog.Theme.Color
import           Nauva.Catalog.Theme.Typeface

import           Prelude hiding (rem)


data TerminalProps = TerminalProps
    { terminalLines :: ![Text]
    }

terminalEl :: TerminalProps -> Element
terminalEl props = div_ [style_ rootStyle] els
  where
    rootStyle = mkStyle $ do
        typeface mono12Typeface
        backgroundColor black
        color "white"
        overflow "auto"
        padding "2rem"

    lineStyle = mkStyle $ do
        whiteSpace "nowrap"
        overflow "hidden"
        cssTerm "text-overflow" "ellipsis"
        opacity "0.9"

    els = (flip map) (terminalLines props) $ \str -> div_ [style_ lineStyle] [str_ str]



catalogPage :: Element
catalogPage = [nauvaCatalogPage|
```nauva
terminalEl $ TerminalProps
    { terminalLines =
        [ "Building nauvad... this may take a while"
        , "nauva-product-nauva-shared-0.0.0: unregistering (local file changes: src/Nauva/Product/Nauva/Element/Message.hs)"
        , "nauvad-0.6.6: unregistering (missing dependencies: nauva-product-nauva-shared)"
        , "nauva-product-nauva-shared-0.0.0: build (lib)"
        , "nauva-product-nauva-shared-0.0.0: copy/register"
        , "nauvad-0.6.6: build (lib + exe)"
        , "nauvad-0.6.6: copy/register"
        , "Completed 2 action(s)."
        ]
    }
```
|]
