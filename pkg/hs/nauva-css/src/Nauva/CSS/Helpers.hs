{-# LANGUAGE OverloadedStrings #-}

module Nauva.CSS.Helpers where


import           Data.Text (Text)
import qualified Data.Text as T

import           Control.Monad.Writer.Lazy

import           Nauva.CSS.Types



vh :: Int -> CSSValue
vh n = CSSValue $ T.pack $ show n ++ "vh"

px :: (Show a) => a -> CSSValue
px n = CSSValue $ T.pack $ show n ++ "px"

rem :: (Show a) => a -> CSSValue
rem n = CSSValue $ T.pack $ show n ++ "rem"

pct :: (Show a) => a -> CSSValue
pct n = CSSValue $ T.pack $ show n ++ "%"



fontFamily_ :: Writer [CSSDeclaration] () -> Writer [Statement] ()
fontFamily_ v = tell [SEmit $ DFontFamily $ execWriter v]


before :: Writer [Statement] () -> Writer [Statement] ()
before style = tell [SSuffix "::before" style]

after :: Writer [Statement] () -> Writer [Statement] ()
after style = tell [SSuffix "::after" style]


onHover :: Writer [Statement] () -> Writer [Statement] ()
onHover style = tell [SSuffix ":hover" style]

onActive :: Writer [Statement] () -> Writer [Statement] ()
onActive style = tell [SSuffix ":active" style]

firstChild :: Writer [Statement] () -> Writer [Statement] ()
firstChild style = tell [SSuffix ":first-child" style]

lastChild :: Writer [Statement] () -> Writer [Statement] ()
lastChild style = tell [SSuffix ":last-child" style]

media :: Text -> Writer [Statement] () -> Writer [Statement] ()
media m style = tell [SCondition (CMedia m) style]
