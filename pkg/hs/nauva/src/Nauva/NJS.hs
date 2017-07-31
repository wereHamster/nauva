{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}

module Nauva.NJS
    ( module Language

    , FID(..), unFID

    , F(..), mkF
    , F1, mkF1
    , F2, mkF2
    , F3, mkF3

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

data F r = F { fId :: !FID, fFn :: !(Exp r) }
instance Eq (F r) where
    (==) = (==) `on` fId

mkF :: Exp r -> F r
mkF exp = createF $ \fId -> F fId exp

type F1 a r = F r
mkF1 :: (Exp a -> Exp r) -> F1 a r
mkF1 exp = mkF (exp (holeE 0))

type F2 a b r = F r
mkF2 :: (Exp a -> Exp b -> Exp r) -> F2 a b r
mkF2 exp = mkF (exp (holeE 0) (holeE 1))

type F3 a b c r = F r
mkF3 :: (Exp a -> Exp b -> Exp c -> Exp r) -> F3 a b c r
mkF3 exp = mkF (exp (holeE 0) (holeE 1) (holeE 2))

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
type FRD a = F (RefHandler a)



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
