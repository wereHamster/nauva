{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}

module Nauva.NJS
    ( module Language

    , FID(..), unFID

    , F0(..), mkF0
    , F1(..), mkF1
    , F2(..), mkF2
    , F3(..), mkF3

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

data F0 r = F0 { f0Id :: !FID, f0Fn :: !(Exp r) }
instance Eq (F0 r) where
    (==) = (==) `on` f0Id

mkF0 :: Exp r -> F0 r
mkF0 exp = createF $ \fId -> F0 fId exp

data F1 a r = F1 { f1Id :: !FID, f1Fn :: !(Exp r) }
instance Eq (F1 a r) where
    (==) = (==) `on` f1Id

mkF1 :: (Exp a -> Exp r) -> F1 a r
mkF1 exp = createF $ \fId -> F1 fId (exp (holeE 0))

data F2 a b r = F2 { f2Id :: !FID, f2Fn :: !(Exp r) }
instance Eq (F2 a b r) where
    (==) = (==) `on` f2Id

mkF2 :: (Exp a -> Exp b -> Exp r) -> F2 a b r
mkF2 exp = createF $ \fId -> F2 fId (exp (holeE 0) (holeE 1))

data F3 a b c r = F3 { f3Id :: !FID, f3Fn :: !(Exp r) }
instance Eq (F3 a b c r) where
    (==) = (==) `on` f3Id

mkF3 :: (Exp a -> Exp b -> Exp c -> Exp r) -> F3 a b c r
mkF3 exp = createF $ \fId -> F3 fId (exp (holeE 0) (holeE 1) (holeE 2))


createF :: (FID -> a) -> a
createF f = unsafePerformIO $ do
    fId <- atomicModifyIORef' fIdCounter $ \i -> (i + 1, i)
    pure $ f $ FID fId


-- | Type synonym for a function which implements an event handler.
type FE ev a = F1 ev (EventHandler a)


-- | A function which is called whenever a ref is attached to a component.
type FRA el a = F1 el (RefHandler a)

-- | Function (when) Ref (is) Detach(ed). You don't get the element which was
-- detached. This means you can't really add the same ref handler to multiple
-- components.
type FRD a = F0 (RefHandler a)



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



data DOMRect

getBoundingClientRect :: Exp a -> Exp DOMRect
getBoundingClientRect element = invokeE (litE "getBoundingClientRect") element []

domRectWidth :: Exp DOMRect -> Exp Float
domRectWidth rect = getE (litE "width") rect

domRectHeight :: Exp DOMRect -> Exp Float
domRectHeight rect = getE (litE "height") rect
