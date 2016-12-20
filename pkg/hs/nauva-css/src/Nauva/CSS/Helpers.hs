{-# LANGUAGE OverloadedStrings #-}

module Nauva.CSS.Helpers where


import           Data.Text (Text)
import qualified Data.Text as T

import           Control.Monad.Writer.Lazy

import           Nauva.CSS.Types



vh :: Int -> CSSValue
vh n = CSSValue $ T.pack $ (show n) ++ "vh"

px :: (Num a, Show a) => a -> CSSValue
px n = CSSValue $ T.pack $ (show n) ++ "px"

rem :: (Num a, Show a) => a -> CSSValue
rem n = CSSValue $ T.pack $ (show n) ++ "rem"

pct :: (Num a, Show a) => a -> CSSValue
pct n = CSSValue $ T.pack $ (show n) ++ "%"



fontFamily_ :: Writer [CSSDeclaration] () -> Writer [Statement] ()
fontFamily_ v = tell [SEmit $ DFontFamily $ execWriter v]


onHover :: Writer [Statement] () -> Writer [Statement] ()
onHover style = tell [SSuffix ":hover" style]

onActive :: Writer [Statement] () -> Writer [Statement] ()
onActive style = tell [SSuffix ":active" style]

media :: Text -> Writer [Statement] () -> Writer [Statement] ()
media m style = tell [SCondition (CMedia m) style]
