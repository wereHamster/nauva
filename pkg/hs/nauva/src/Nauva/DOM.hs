{-|
Types which are needed to represent DOM-related objects which Nauva uses. Many
of these already exist in the 'blaze-markup' or 'blaze-html' packages. However,
those types are not suited for use within 'Nauva', so we define our own.

 - 'Tag' -- The tag of a DOM node. Used in 'ENode', 'INode', and 'SNode'.
 - 'Attribute' and 'AttributeValue' -- Attributes which are attached to DOM nodes.

-}

module Nauva.DOM
    ( -- * Tag
      Tag(..)

      -- * Attribute
    , Attribute(..)
    , AttributeValue(..)

      -- ** Attribute constructors
      -- $attributeValueConstructors
    , boolAttribute
    , stringAttribute
    , intAttribute
    , doubleAttribute
    ) where


import           Data.Aeson
import           Data.String   (IsString(..))
import           Data.Function
import           Data.Text     (Text)
import qualified Data.Text     as T

import           Control.Applicative

import           Prelude



--------------------------------------------------------------------------------
-- | DOM tag. We provide a 'IsString' instance for convenience, use it
-- judiciously.
--
-- In any application, by far the most common 'Tag' will be @Tag "div"@, or
-- simpler, if using the OverloadedStrings extension, just @"div"@. Thus the
-- two following forms are equivalent:
--
-- > ENode "div"       Nothing [] [] ...
-- > ENode (Tag "div") Nothing [] [] ...

newtype Tag = Tag { unTag :: Text }
    deriving (Eq, Ord)

instance Show Tag where
    show = T.unpack . unTag

instance IsString Tag where
    fromString = Tag . T.pack

instance ToJSON Tag where
    toJSON = toJSON . unTag

instance FromJSON Tag where
    parseJSON x = Tag <$> parseJSON x



--------------------------------------------------------------------------------
-- | We try to model attributes after IDL attributes (see the link below for
-- the difference between content attributes and IDL attributes). That means
-- we don't treat 'attributeValue' as a simple string, but instead explicitly
-- differentiate between the different types (String, Bool, Int, URL etc).
--
-- https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes

data Attribute
    = AVAL !Text !AttributeValue
    deriving (Eq, Ord)

instance ToJSON Attribute where
    toJSON (AVAL name value) = toJSON (name, value)

instance FromJSON Attribute where
    parseJSON v = do
        (name, value) <- parseJSON v
        pure $ AVAL name value



--------------------------------------------------------------------------------
data AttributeValue
    = AVBool !Bool
    | AVString !Text
    | AVInt !Int
    | AVDouble !Double
    deriving (Eq, Ord)


instance IsString AttributeValue where
    fromString = AVString . T.pack

instance ToJSON AttributeValue where
    toJSON (AVBool   b) = toJSON b
    toJSON (AVString s) = toJSON s
    toJSON (AVInt    i) = toJSON i
    toJSON (AVDouble d) = toJSON d

instance FromJSON AttributeValue where
    parseJSON   (Bool   b) = pure $ AVBool b
    parseJSON   (String s) = pure $ AVString s
    parseJSON v@(Number _) = (AVInt <$> parseJSON v) <|> (AVDouble <$> parseJSON v)
    parseJSON    _         = fail "AttributeValue"


-- $attributeValueConstructors
-- These constructors are here for convenience. You are encouraged to use
-- these functions instead of the 'Attribute' and 'AttributeValue'
-- constructors.

boolAttribute :: Text -> Bool -> Attribute
boolAttribute name value = AVAL name (AVBool value)

stringAttribute :: Text -> Text -> Attribute
stringAttribute name value = AVAL name (AVString value)

intAttribute :: Text -> Int -> Attribute
intAttribute name value = AVAL name (AVInt value)

doubleAttribute :: Text -> Double -> Attribute
doubleAttribute name value = AVAL name (AVDouble value)
