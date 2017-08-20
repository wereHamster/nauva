{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}

module Nauva.Catalog.Specimens.TypefaceSpecimen
    ( typefaceSpecimen
    , typefaceSpecimen'
    ) where


import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.Monoid
import           Data.Typeable
import           Data.Data
import           Data.List
import           Data.Color
import           Data.Word
import qualified Data.Aeson as A

import qualified Text.Blaze.Html     as B
import qualified Text.Blaze.Internal as B
import qualified Text.Blaze.Html.Renderer.Pretty as B

import           Control.Monad
import           Control.Lens hiding (none)

import           Language.Haskell.TH.Syntax

import           Nauva.View
import           Nauva.Catalog.Theme.Typeface
import           Nauva.Static
import           Nauva.Catalog.Elements



typefaceSpecimen :: Text -> Typeface -> Element
typefaceSpecimen t tf = pageElement
    PageElementProps{pepTitle = Nothing, pepSpan = 6}
    [ div_ [style_ rootStyle]
        [ div_ [style_ metaStyle] [str_ $ tfName tf <> " – " <> metaString]
        , div_ [style_ previewStyle] [str_ t]
        ]
    ]
  where
    rootStyle = mkStyle $ do
        display flex
        flexDirection column

    metaStyle = mkStyle $ do
        typeface meta14Typeface
        color "black"
        marginBottom "4px"
        color "#333"

    metaString = mconcat $ intersperse ", "
        [ unCSSValue (tfFontFamily tf)
        , unCSSValue (tfFontWeight tf)
        , unCSSValue (tfFontSize tf) <> "/" <> unCSSValue (tfLineHeight tf)
        ]

    previewStyle = mkStyle $ do
        typeface tf
        padding "20px"
        backgroundColor "white"
        border "1px solid #eee"

typefaceSpecimen' :: Typeface -> Element
typefaceSpecimen' = typefaceSpecimen "A very bad quack might jinx zippy fowls"
