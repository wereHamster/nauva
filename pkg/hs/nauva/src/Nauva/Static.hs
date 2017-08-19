{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
This module contains function to convert 'Element's or 'Instance's into static
markup (blaze-markup / blaze-html). Useful when you want to render your
application on the server to generate static web pages.

The functions generate 'Text.Blaze.Html' markup. You can convert that further
into 'String', 'Text', 'ByteString' etc by using functions from the
blaze-html 'Text.Blaze.Html.Renderer' modules.
-}

module Nauva.Static
    ( elementToMarkup
    , instanceToMarkup
    ) where


import           Data.Monoid
import           Data.Maybe
import qualified Data.Text as T
import           Data.String
import           Data.List (intersperse)

import           Control.Monad

import qualified Text.Blaze.Html     as B
import qualified Text.Blaze.Internal as B

import           Control.Concurrent.STM

import           Nauva.Internal.Types
import           Nauva.CSS
import           Nauva.DOM

import           Prelude


-- | Convert an 'Element' into blaze-html markup ('B.Html') and a list of all
-- 'Style' objects which are used by the tree. If the 'Element' tree contains
-- 'Component's, they will be rendered in their initial state.
--
-- Note that the function was cobbled together without a thorough understanding
-- of the 'blaze-markup' and 'blaze-html' types. Furthermore, I know for a fact
-- that the way how Nauva 'Attribute's are converted into blaze-markup
-- Attributes is not accurate: 'Nauva' models them after IDL attributes, while
-- 'blaze-html' uses content attributes.

elementToMarkup :: Element -> STM (B.Html, [Style], [IO ()])
elementToMarkup el = case el of
    ENull ->
        pure (mempty, mempty, [])

    (EText text) ->
        pure (B.toMarkup text, mempty, [])

    (ENode tag attributes children) -> do
        let tagString = T.unpack $ unTag tag
            parent = B.Parent (fromString tagString) (fromString $ "<" <> tagString) (fromString $ "</" <> tagString <> ">")
            styles = mapMaybe toStyle attributes
              where
                toStyle (ASTY x) = Just x
                toStyle _ = Nothing

            classes = map (("s"<>) . unHash . cssRuleHash) $ mconcat $ map unStyle styles
            attrs = mapMaybe toAttribute attributes
            toAttribute (AEVL _) = Nothing
            toAttribute (ASTY _) = Nothing
            toAttribute (AREF _) = Nothing
            toAttribute (AVAL n v) = Just $ B.attribute (B.textTag n) (B.textTag $ " " <> n <> "=\"") $ case v of
                AVBool b -> B.textValue $ if b then "true" else "false"
                AVString t -> B.textValue t
                AVInt i -> B.stringValue $ show i
                AVDouble d -> B.stringValue $ show d
            parentWithAttributes = foldl (B.!) parent attrs

        (children', childrenStyles, childrenActions) :: (B.Html, [Style], [IO ()]) <- mconcat <$> mapM elementToMarkup children
        let html = if null classes
                then parentWithAttributes children'
                else parentWithAttributes children' B.! B.attribute (B.textTag "class") (B.textTag " class=\"") (B.textValue (mconcat (intersperse " " classes)))
        pure (html, styles <> childrenStyles, childrenActions)

    (EThunk thunk p) ->
        elementToMarkup $ forceThunk thunk p

    (EComponent component p) -> do
        (s, _, actions) <- initialComponentState component p
        (html, styles, innerActions) <- elementToMarkup $ renderComponent component p s
        pure (html, styles, map void actions <> innerActions)



-- | Not implemented yet!
instanceToMarkup :: Instance -> STM B.Html
instanceToMarkup _inst = error "instanceToMarkup: Not implemented yet!"
