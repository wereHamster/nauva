{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module Nauva.CSS.Types where


import           Data.Aeson (ToJSON(..), FromJSON(..))
import qualified Data.Aeson as A
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.String
import           Data.ByteString.Lazy (toStrict)

import           Control.Monad.Writer.Lazy

import           Crypto.MAC.SipHash (SipHash(..), SipKey(..))
import qualified Crypto.MAC.SipHash as SH

import           Prelude



-------------------------------------------------------------------------------
-- | These values can appear as the right-hand-side of a CSS declaration. For
-- example in @color: magenta@ the string @magenta@ would be a 'CSSValue'.
--
-- There is a 'IsString' instance to make it easier to create custom values for
-- which we don't have a combinator or helper function yet.
--
-- > let widthValue = "calc(100% - 20px)" :: CSSValue

newtype CSSValue = CSSValue { unCSSValue :: Text }

instance Show CSSValue where
    show = show . unCSSValue

instance ToJSON CSSValue where
    toJSON = toJSON . unCSSValue

instance FromJSON CSSValue where
    parseJSON x = CSSValue <$> parseJSON x

instance IsString CSSValue where
    fromString = CSSValue . T.pack



-------------------------------------------------------------------------------
-- | A basic CSS declaration (property-value pair) whose value isn't processed
-- any further and taken at face value. If you put garbage in, you'll get
-- garbage out. These pairs are written verbatim into the output file or
-- a StyleSheet object in the browser.

type CSSDeclaration = (Text, CSSValue)



-------------------------------------------------------------------------------
-- | A block of CSS declarations. Equivalent to stuff between curly braces in
-- plain CSS. Such blocks may appear as bodies of 'CSSStyleRule',
-- 'CSSFontFaceRule' etc.
--
-- Keys can appear multiple times, with the the last one overwriting all previous
-- occurrences.

type CSSStyleDeclaration = [CSSDeclaration]



-------------------------------------------------------------------------------
-- | Mimics a IDL CSSRule. Though only some of the variants are supported.
--
-- In addition to the stuff which is needed to generate the CSS text, it also
-- contains a unique 'Hash' which can be used to quickly compare two objects
-- or track which ones have been inserted into the DOM to avoid inserting
-- the same 'CSSRule' multiple times.

data CSSRule
    = CSSStyleRule !Hash ![Condition] ![Suffix] !CSSStyleDeclaration
    | CSSFontFaceRule !Hash !CSSStyleDeclaration
    deriving (Show)

instance A.ToJSON CSSRule where
    toJSON (CSSStyleRule hash conditions suffixes styleDeclaration) = A.toJSON
        [ A.toJSON (1 :: Int)
        , A.toJSON hash
        , A.toJSON conditions
        , A.toJSON suffixes
        , A.toJSON $ M.fromList styleDeclaration
        ]
    toJSON (CSSFontFaceRule hash styleDeclaration) = A.toJSON
        [ A.toJSON (5 :: Int)
        , A.toJSON hash
        , A.toJSON $ M.fromList styleDeclaration
        ]



-------------------------------------------------------------------------------
-- | Statements are the lowest-level building blocks of a CSS style. Users
-- of this library don't deal with this though. There are combinators and helper
-- functions to create values of this type.

data Statement
    = SEmit !Declaration
      -- ^ Emit a single CSS declaration into the current context.
    | SCondition !Condition !(Writer [Statement] ())
      -- ^ Wrap a block in a condition (@media or @supports).
    | SSuffix !Suffix !(Writer [Statement] ())
      -- ^ Wrap a block in a suffix (pseudo selector or pseudo class).



-------------------------------------------------------------------------------
-- | Similar to 'CSSDeclaration' but has special support for certain
-- properties. These properties are expanded while generating the output and
-- may cause additional CSS rules to be emitted.

data Declaration
    = DPlain !Text !CSSValue
    | DFontFamily !CSSStyleDeclaration
    deriving (Show)



-------------------------------------------------------------------------------
-- | A 'Style' is the maximally prepared, preprocessed, precompiled object we
-- can create at compile time. Any remaining transformations need to be
-- applied at runtime, because they depend on the host (browser).

newtype Style = Style { unStyle :: [CSSRule] }

instance A.ToJSON Style where
    toJSON = A.toJSON . unStyle


mkStyle :: Writer [Statement] () -> Style
mkStyle = Style . execWriter . writeRules . M.toList . flatten . execWriter
  where
    -- Convert a list of statements into unique declaration blocks (unique by the
    -- context, which is the list of suffixes for now).
    flatten :: [Statement] -> Map ([Condition], [Suffix]) [Declaration]
    flatten = foldl (go ([],[])) mempty
      where
        go k       m (SEmit decl)     = M.insertWith (flip (<>)) k [decl] m
        go (cs,ss) m (SCondition c n) = foldl (go (cs <> [c], ss)) (M.insert (cs <> [c], ss) [] m) (execWriter n)
        go (cs,ss) m (SSuffix s n)    = foldl (go (cs, ss <> [s])) (M.insert (cs, ss <> [s]) [] m) (execWriter n)

    -- Convert a list of declaration blocks into a list of CSS rules. One declaration block
    -- may map to multiple CSS rules (in the presence of DFontFamily and other special
    -- declaration types).
    writeRules :: [(([Condition], [Suffix]), [Declaration])] -> Writer [CSSRule] ()
    writeRules [] = pure ()
    writeRules (((conditions, suffixes), decls):xs) = do
        styleDeclaration <- forM decls $ \decl -> case decl of
            (DPlain property value) -> pure (property, value)
            (DFontFamily ffdecl) -> do
                let hash = cssStyleDeclarationHash ffdecl

                -- If the original block doesn't contain a "font-family" declaration,
                -- generate one from the block hash.
                let (fontFamily, ffdecl') = case lookup "font-family" ffdecl of
                        Nothing -> let ff = CSSValue ("f-" <> unHash hash) in (ff, ffdecl <> [("font-family", ff)])
                        Just ff -> (ff, ffdecl)

                tell [CSSFontFaceRule hash ffdecl']
                pure ("font-family", fontFamily)

        let hash = cssStyleDeclarationHash styleDeclaration
        tell [CSSStyleRule hash conditions suffixes styleDeclaration]

        writeRules xs

noStyle :: Style
noStyle = Style []



-------------------------------------------------------------------------------
-- | For the purposes of nauva-css, a 'Hash' is simply a newtype around 'Text'.
-- It is derived from a 'CSSStyleDeclaration' (which is actually just a list
-- of 'CSSDeclaration').

newtype Hash = Hash { unHash :: Text }
    deriving (Eq, Ord)

instance Show Hash where
    show = show . unHash

instance A.ToJSON Hash where
    toJSON = A.toJSON . unHash


cssStyleDeclarationHash :: CSSStyleDeclaration -> Hash
cssStyleDeclarationHash = Hash . T.pack . show . unSipHash . SH.hash sipKey . toStrict . A.encode . toJSON
  where
    sipKey = SipKey 0 1
    unSipHash (SipHash x) = x



-------------------------------------------------------------------------------
-- | A condition under which a 'CSSStyleRule' should be active. Corresponds to
-- @media and @supports conditional group rules.
--
-- NB. There is also @document, but it's weird and we don't support that.

data Condition
    = CMedia !Text
    | CSupports !Text
    deriving (Eq, Ord)

instance Show Condition where
    show (CMedia    x) = "@media(" <> T.unpack x <> ")"
    show (CSupports x) = "@supports(" <> T.unpack x <> ")"

instance A.ToJSON Condition where
    toJSON (CMedia    x) = A.toJSON [A.toJSON (1 :: Int), A.toJSON x]
    toJSON (CSupports x) = A.toJSON [A.toJSON (2 :: Int), A.toJSON x]



-------------------------------------------------------------------------------
-- | A suffix is a pseudo-class or pseudo-element.
--
-- The inner text includes any leading colons that are required by the selector.

newtype Suffix = Suffix { unSuffix :: Text }
    deriving (Eq, Ord)

instance Show Suffix where
    show = show . unSuffix

instance A.ToJSON Suffix where
    toJSON = A.toJSON . unSuffix

instance IsString Suffix where
    fromString = Suffix . T.pack



-------------------------------------------------------------------------------

class CSSTerm a where
    cssTerm :: Text -> a

instance CSSTerm CSSValue where
    cssTerm = CSSValue

instance (a ~ (), v1 ~ CSSValue) => CSSTerm (v1 -> Writer [Statement] a) where
    cssTerm property v1 = tell [SEmit $ DPlain property v1]

instance (a ~ (), v1 ~ CSSValue, v2 ~ CSSValue) => CSSTerm (v1 -> v2 -> Writer [Statement] a) where
    cssTerm property (CSSValue v1) (CSSValue v2) = tell [SEmit $ DPlain property $ CSSValue $ v1 <> " " <> v2]

instance (a ~ (), v1 ~ CSSValue, v2 ~ CSSValue, v3 ~ CSSValue) => CSSTerm (v1 -> v2 -> v3 -> Writer [Statement] a) where
    cssTerm property (CSSValue v1) (CSSValue v2) (CSSValue v3) = tell [SEmit $ DPlain property $ CSSValue $ v1 <> " " <> v2 <> " " <> v3]

instance (a ~ (), v1 ~ CSSValue, v2 ~ CSSValue, v3 ~ CSSValue, v4 ~ CSSValue) => CSSTerm (v1 -> v2 -> v3 -> v4 -> Writer [Statement] a) where
    cssTerm property (CSSValue v1) (CSSValue v2) (CSSValue v3) (CSSValue v4) = tell [SEmit $ DPlain property $ CSSValue $ v1 <> " " <> v2 <> " " <> v3 <> " " <> v4]

instance (a ~ (), v ~ CSSValue) => CSSTerm (v -> Writer [CSSDeclaration] a) where
    cssTerm property value = tell [(property, value)]
