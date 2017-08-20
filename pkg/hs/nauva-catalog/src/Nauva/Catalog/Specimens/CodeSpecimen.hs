{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}

module Nauva.Catalog.Specimens.CodeSpecimen
    ( CodeSpecimenProps(..)
    , codeSpecimen
    ) where


import           Data.Text          (Text)
import           Data.Typeable
import           Data.Data
import qualified Data.Aeson          as A

import           Language.Haskell.TH.Syntax

import           Nauva.View
import           Nauva.Catalog.Theme.Typeface
import           Nauva.Catalog.Elements



data CodeSpecimen = CodeSpecimen
    { csProps :: CodeSpecimenProps
    , csElement :: Element
    , csLanguage :: Text
    , csSource :: Text
    }

data CodeSpecimenProps = CodeSpecimenProps
    { cspPEP :: PageElementProps
    , cspNoSource :: Bool
    } deriving (Typeable, Data, Lift)

instance A.FromJSON CodeSpecimenProps where
    parseJSON v@(A.Object o) = CodeSpecimenProps
        <$> A.parseJSON v
        <*> o A..:? "noSource" A..!= False

    parseJSON _ = fail "CodeSpecimenProps"


codeSpecimen :: CodeSpecimen -> Element
codeSpecimen CodeSpecimen{..} = if cspNoSource
    then div_ [style_ rootStyle] [pageElementContainer [csElement]]
    else div_ [style_ rootStyle] [pageElementContainer [csElement], codeBlock csLanguage csSource]
  where
    CodeSpecimenProps{..} = csProps
    rootStyle = mkStyle $ do
        typeface mono12Typeface
        fontStyle "normal"
        fontWeight "400"
        color "rgb(51, 51, 51)"
        display "block"
        width "100%"
        background "rgb(255, 255, 255)"
        border "1px solid rgb(238, 238, 238)"
