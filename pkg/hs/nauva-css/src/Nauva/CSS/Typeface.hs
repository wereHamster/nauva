{-# LANGUAGE RecordWildCards #-}

module Nauva.CSS.Typeface
    ( Typeface(..)
    , typeface
    , typeFace
    ) where


import Data.Text (Text)
import Nauva.CSS.Terms
import Nauva.CSS.Types
import Control.Monad.Writer.Lazy



-- | A 'Typeface' is a combination of font family, font weight, font size,
-- line height and font variation. The four properties together identify
-- a particular type face which is used in one or more places inside your
-- application.
--
-- You should not define new type faces ad-hoc inside individual components.
-- Instead, define them once in a single place and then reuse. This allows
-- you to manage an index of all typefaces which are used throughout your
-- application, show them all in the catalog, and prevents the number
-- of distinct typefaces from exploding.
--
-- A 'Typeface' also has a name, which should be unique and is only used
-- in the catalog, for documentation purposes.
--
-- Note: the font family is specified using a 'CSSValue'. This means custom
-- @font-face is not supported at the moment (see 'fontFamily_' or 'DFontFamily').

data Typeface = Typeface
    { tfName :: Text
    , tfFontFamily :: CSSValue
    , tfFontWeight :: CSSValue
    , tfFontSize :: CSSValue
    , tfLineHeight :: CSSValue
    }


-- | Apply the given 'Typeface' in a CSS block.
--
-- > someTypeface = Typeface "brandBodyCopy" "Helvetica, sans-serif" "normal" "16px" "1.4"
--
-- > rootStyle = mkStyle $ do
-- >     typeface someTypeface
-- >     color "black"

typeface :: Typeface -> Writer [Statement] ()
typeface (Typeface {..}) = do
    fontFamily tfFontFamily
    fontWeight tfFontWeight
    fontSize tfFontSize
    lineHeight tfLineHeight

{-# DEPRECATED typeFace "Use typeface instead of typeFace (with capital F)" #-}
typeFace = typeface
