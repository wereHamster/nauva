{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}

module Nauva.NJS
    ( FID(..), unFID

    , F(..), mkF
    , F1, mkF1
    , F2, mkF2
    , F3, mkF3

    , FE
    , FRA, FRD

    , Value(..)
    ) where


import           Data.Function
import           Data.IORef
import qualified Data.Aeson       as A
import qualified Data.Aeson.Types as A
import           Data.Text        (Text)

import           System.IO.Unsafe



--------------------------------------------------------------------------------
-- | FID - Function ID
--
-- The 'FID' is used to uniquely identify NJS function expressions. The
-- constructor is private, only a smart constructor ('mkFID') is exported.
-- 'mkFID' ensures that the 'FID' is globally unique.

newtype FID = FID Int
    deriving (Eq)

instance A.ToJSON FID where
    toJSON = A.toJSON . unFID

fIdCounter :: IORef Int
fIdCounter = unsafePerformIO $ newIORef 1
{-# NOINLINE fIdCounter #-}

-- Q: Why is this exported? A: So we can implement the ToJSVal instance.
unFID :: FID -> Int
unFID (FID x) = x



--------------------------------------------------------------------------------
-- Function expressions with fixed arity.

data F r = F
    { fId :: !FID
    , fConstructors :: ![(Text,[Text])]
      -- ^ Action constructors which are used by the function body.
      -- (constructor name, [argument types])
    , fArguments :: ![(Text,Text)]
      -- ^ Arguments which the function body requires.
      -- (binding name, W3C DOM type)
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


mkF :: [(Text,Text)] -> Text -> F r
mkF args body = createF $ \fId -> F
    { fId = fId
    , fConstructors = []
    , fArguments = args
    , fBody = body
    }

type F1 a r = F r
mkF1 :: (Text,Text) -> Text -> F1 a r
mkF1 a body = mkF [a] body

type F2 a b r = F r
mkF2 :: (Text,Text) -> (Text,Text) -> Text -> F2 a b r
mkF2 a b body = mkF [a, b] body

type F3 a b c r = F r
mkF3 :: (Text,Text) -> (Text,Text) -> (Text,Text) -> Text -> F3 a b c r
mkF3 a b c body = mkF [a, b, c] body

createF :: (FID -> a) -> a
createF f = unsafePerformIO $ do
    fId <- atomicModifyIORef' fIdCounter $ \i -> (i + 1, i)
    pure $ f $ FID fId


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
