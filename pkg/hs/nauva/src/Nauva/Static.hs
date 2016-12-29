{-# LANGUAGE OverloadedStrings #-}

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
import qualified Data.Text as T
import           Data.String

import qualified Text.Blaze.Html     as B
import qualified Text.Blaze.Internal as B

import           Control.Concurrent.STM

import           Nauva.Internal.Types
import           Nauva.DOM

import           Prelude


-- | Convert an 'Element' into blaze-html markup ('B.Html'). If the 'Element'
-- tree contains 'Component's, they will be rendered in their initial state.
--
-- Note that the function was cobbled together without a thorough understanding
-- of the 'blaze-markup' and 'blaze-html' types. Furthermore, I know for a fact
-- that the way how Nauva 'Attribute's are converted into blaze-markup
-- Attributes is not accurate: 'Nauva' models them after IDL attributes, while
-- 'blaze-html' uses content attributes.
elementToMarkup :: Element -> STM B.Html
elementToMarkup el = case el of
    (EText text) ->
        pure $ B.toMarkup text

    (ENode tag attributes children) ->
        let tagString = T.unpack $ unTag tag
            parent = B.Parent (fromString tagString) (fromString $ "<" <> tagString) (fromString $ "</" <> tagString <> ">")
            attrs = map toAttribute attributes
            toAttribute (AVAL n v) = B.attribute (B.textTag n) (B.textTag $ " " <> n <> "=\"") $ case v of
                AVBool b -> B.textValue $ if b then "true" else "false"
                AVString t -> B.textValue t
                AVInt i -> B.stringValue $ show i
                AVDouble d -> B.stringValue $ show d
            parentWithAttributes = foldl (\a b -> a B.! b) parent attrs
        in parentWithAttributes <$> (mconcat <$> mapM elementToMarkup children)

    (EThunk thunk p) ->
        elementToMarkup $ forceThunk thunk p

    (EComponent component p) -> do
        (s, _) <- initialComponentState component p
        elementToMarkup $ renderComponent component s


-- | Not implemented yet!
instanceToMarkup :: Instance -> STM B.Html
instanceToMarkup _inst = error "instanceToMarkup: Not implemented yet!"
