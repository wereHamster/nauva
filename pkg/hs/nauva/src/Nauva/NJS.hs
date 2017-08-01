{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}

module Nauva.NJS
    ( FID(..), unFID

    , createF
    , F(..), mkF
    , F1, mkF1
    , F2, mkF2
    , F3, mkF3

    , FE
    , FRA, FRD

    , Value(..)
    ) where


import           Data.Function
import qualified Data.Aeson       as A
import qualified Data.Aeson.Types as A
import           Data.Text        (Text)
import qualified Data.Text        as T
import           Data.ByteString.Lazy (toStrict)

import           Crypto.MAC.SipHash (SipHash(..), SipKey(..))
import qualified Crypto.MAC.SipHash as SH



--------------------------------------------------------------------------------
-- | FID - Function ID
--
-- The 'FID' is used to uniquely identify NJS function expressions. The
-- constructor is private, only a smart constructor ('mkFID') is exported.
-- 'mkFID' ensures that the 'FID' is globally unique.

newtype FID = FID Text
    deriving (Eq)

instance A.ToJSON FID where
    toJSON = A.toJSON . unFID

-- Q: Why is this exported? A: So we can implement the ToJSVal instance.
unFID :: FID -> Text
unFID (FID x) = x



--------------------------------------------------------------------------------
-- Function expressions with fixed arity.

data F r = F
    { fId :: !FID
    , fConstructors :: ![Text]
      -- ^ Action constructors which are used by the function body.
    , fArguments :: ![Text]
      -- ^ Arguments which the function body requires.
    , fBody :: !Text
      -- ^ JavaScript code of the function body. This string is passed to
      -- @new Function(…)@.
    }

instance Eq (F r) where
    (==) = (==) `on` fId

instance A.ToJSON (F r) where
    toJSON f = A.object
        [ "id"           A..= fId f
        , "constructors" A..= fConstructors f
        , "arguments"    A..= fArguments f
        , "body"         A..= fBody f
        ]


mkF :: [Text] -> Text -> F r
mkF args body = createF [] args body

type F1 a r = F r
mkF1 :: Text -> Text -> F1 a r
mkF1 a body = mkF [a] body

type F2 a b r = F r
mkF2 :: Text -> Text -> Text -> F2 a b r
mkF2 a b body = mkF [a, b] body

type F3 a b c r = F r
mkF3 :: Text -> Text -> Text -> Text -> F3 a b c r
mkF3 a b c body = mkF [a, b, c] body

createF :: [Text] -> [Text] -> Text -> F r
createF constructors arguments body = F
    { fId           = hash $ A.toJSON [A.toJSON constructors, A.toJSON arguments, A.toJSON body]
    , fConstructors = constructors
    , fArguments    = arguments
    , fBody         = body
    }
  where
    hash = FID . T.pack . show . unSipHash . SH.hash sipKey . toStrict . A.encode
    sipKey = SipKey 0 1
    unSipHash (SipHash x) = x


-- | Type synonym for a function which implements an event handler.
type FE ev a = F1 ev a


-- | A function which is called whenever a ref is attached to a component.
type FRA el a = F1 el a

-- | Function (when) Ref (is) Detach(ed). You don't get the element which was
-- detached. This means you can't really add the same ref handler to multiple
-- components.
type FRD a = F a



--------------------------------------------------------------------------------

class Value a where
    parseValue :: A.Value -> A.Parser a

instance Value () where
    parseValue _ = pure () -- A.parseJSON

instance Value Int where
    parseValue = A.parseJSON

instance Value Float where
    parseValue = A.parseJSON

instance Value Text where
    parseValue = A.parseJSON

instance (Value a, Value b) => Value (a,b) where
    parseValue v = do
      list <- A.parseJSON v
      case list of
        [a,b] -> (,) <$> parseValue a <*> parseValue b
        _     -> fail "(,)"
