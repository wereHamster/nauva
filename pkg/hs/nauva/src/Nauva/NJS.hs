{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}

module Nauva.NJS
    ( module Language

    , FID(..), unFID

    , F1(..), F2(..), F3(..)
    , createF

    , FE
    , FRA, FRD

    , Value(..)

    , getBoundingClientRect
    , domRectWidth, domRectHeight
    ) where


import           Data.Function
import           Data.IORef
import qualified Data.Aeson       as A
import qualified Data.Aeson.Types as A
import           Data.Text        (Text)
import           Data.Tagged

import           System.IO.Unsafe


import qualified Nauva.NJS.Language as Language
import           Nauva.NJS.Language



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

data F1 a r = F1 { f1Id :: FID, f1Fn :: Exp a -> Exp r }
instance Eq (F1 a r) where
    (==) = (==) `on` f1Id

data F2 a b r = F2 { f2Id :: FID, f2Fn :: Exp a -> Exp b -> Exp r }
instance Eq (F2 a b r) where
    (==) = (==) `on` f2Id

data F3 a b c r = F3 { f3Id :: FID, f3Fn :: Exp a -> Exp b -> Exp c -> Exp r }
instance Eq (F3 a b c r) where
    (==) = (==) `on` f3Id


createF :: (FID -> a) -> a
createF f = unsafePerformIO $ do
    fId <- atomicModifyIORef' fIdCounter $ \i -> (i + 1, i)
    pure $ f $ FID fId


-- | Type synonym for a function which implements an event handler.
type FE ev a = F1 ev (EventHandler a)


-- | A function which is called whenever a ref is attached to a component.
type FRA c el a = F2 () el (RefHandler a)

-- | Function (when) Ref (is) Detach(ed). You don't get the element which was
-- detached. This means you can't really add the same ref handler to multiple
-- components.
type FRD c a = F1 () (RefHandler a)



--------------------------------------------------------------------------------

class Value a where
    parseValue :: Tagged a A.Value -> A.Parser a

instance Value () where
    parseValue _ = pure () -- A.parseJSON . unTagged

instance Value Int where
    parseValue = A.parseJSON . unTagged

instance Value Float where
    parseValue = A.parseJSON . unTagged

instance Value Text where
    parseValue = A.parseJSON . unTagged

instance (Value a, Value b) => Value (a,b) where
    parseValue v = do
      list <- A.parseJSON (unTagged v)
      case list of
        [a,b] -> (,) <$> parseValue (Tagged a) <*> parseValue (Tagged b)
        _     -> fail "(,)"



data DOMRect

getBoundingClientRect :: Exp a -> Exp DOMRect
getBoundingClientRect element = invokeE (litE "getBoundingClientRect") element []

domRectWidth :: Exp DOMRect -> Exp Float
domRectWidth rect = getE (litE "width") rect

domRectHeight :: Exp DOMRect -> Exp Float
domRectHeight rect = getE (litE "height") rect
