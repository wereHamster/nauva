{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Nauva.NJS.Language
    ( Exp(..)
    , SomeExp(..)

    , CTag(..)

    , Con0(..), njsCon0
    , Con1(..), njsCon1
    , Con2(..), njsCon2
    , Con3(..), njsCon3

    , Lit(..)
    , litE

    , holeE
    , getE
    , invokeE
    , derefE

    , value0E
    , value1E
    , value2E


    , EventHandler
    , eventHandlerE

    , RefHandler
    , refHandlerE

    , justE
    , nothingE

    ) where


import qualified Data.Aeson as A
import           Data.Text (Text)
import           Data.Monoid



--------------------------------------------------------------------------------

data Exp a where

    UnitE :: Exp ()


    -- A hole which is filled in by whoever is evaluating the expression. If an
    -- expression has a 'HoleE' whose filling is not provided during evaluation,
    -- then that's an error.
    --
    -- Holes are numbered. The number identifies the hole. That means,
    -- expressions can refer to multiple distinct arguments, and can use holes
    -- which refer to the same value multiple times.

    HoleE :: Int -> Exp a


    -- A literal Haskell value embedded in the expression. Use 'lift' to
    -- lift Haskell values into 'Exp'. In fact, never use this constructor
    -- directly!

    LitE :: Lit a -> Exp a


    -- The global object of the JavaScript execution context. If you want to
    -- invoke global functions, use this as the first argument of the
    -- Invoke constructors.

    GlobalE :: Exp a


    -- Get a value out of an object. Compiles down to @o[k]@ in JavaScript.
    --
    -- https://tc39.github.io/ecma262/2016/#sec-get

    GetE :: Exp Text -> Exp o -> Exp a


    -- Invoke a function with an arbitrary number of arguments.
    -- JavaScript: @o[k](...)@
    --
    -- https://tc39.github.io/ecma262/2016/#sec-invoke

    InvokeE :: Exp Text -> Exp v -> [SomeExp] -> Exp a


    -- Construct a value with zero or more arguments. The corresponding 'Value'
    -- instance must match the number and type of arguments.

    Value0E :: Con0 a -> Exp a
    Value1E :: Con1 x a -> Exp x -> Exp a
    Value2E :: Con2 x y a -> Exp x -> Exp y -> Exp a
    Value3E :: Con3 x y z a -> Exp x -> Exp y -> Exp z -> Exp a

    EventHandlerE :: Exp Bool -> Exp Bool -> Exp Bool -> Exp (Maybe a) -> Exp (EventHandler a)
    RefHandlerE :: Exp (Maybe a) -> Exp (RefHandler a)
    DerefE :: Int -> Exp a

    JustE :: Exp a -> Exp (Maybe a)
    NothingE :: Exp (Maybe a)

instance A.ToJSON (Exp a) where
    toJSON (UnitE)
        = A.toJSON ()

    toJSON (HoleE i)
        = A.toJSON [A.String "HoleE", A.toJSON i]

    toJSON GlobalE
        = A.toJSON [A.String "GlobalE"]

    toJSON (LitE lit)
        = A.toJSON [A.String "LitE", A.toJSON lit]

    toJSON (GetE o k)
        = A.toJSON [A.String "GetE", A.toJSON o, A.toJSON k]

    toJSON (InvokeE v k args)
        = A.toJSON $ [A.String "InvokeE", A.toJSON v, A.toJSON k] <> map A.toJSON args

    toJSON (Value0E (Con0 ctag _))
        = A.toJSON $ [A.String "Value0E", A.toJSON ctag]
    toJSON (Value1E (Con1 ctag _) a)
        = A.toJSON $ [A.String "Value1E", A.toJSON ctag, A.toJSON a]
    toJSON (Value2E (Con2 ctag _) a b)
        = A.toJSON $ [A.String "Value2E", A.toJSON ctag, A.toJSON a, A.toJSON b]
    toJSON (Value3E (Con3 ctag _) a b c)
        = A.toJSON $ [A.String "Value3E", A.toJSON ctag, A.toJSON a, A.toJSON b, A.toJSON c]

    toJSON (EventHandlerE a b c d)
        = A.toJSON $ [A.String "EventHandlerE", A.toJSON a, A.toJSON b, A.toJSON c, A.toJSON d]

    toJSON (RefHandlerE a)
        = A.toJSON $ [A.String "RefHandlerE", A.toJSON a]

    toJSON (DerefE refKey)
        = A.toJSON $ [A.String "DerefE", A.toJSON refKey]

    toJSON (JustE v)
        = A.toJSON $ [A.String "JustE", A.toJSON v]

    toJSON (NothingE)
        = A.toJSON $ [A.String "NothingE"]



--------------------------------------------------------------------------------

data SomeExp where
    SomeExp :: Exp a -> SomeExp

instance A.ToJSON SomeExp where
    toJSON (SomeExp e) = A.toJSON e



--------------------------------------------------------------------------------

data Lit a where
    StringL :: Text -> Lit Text
    IntL :: Int -> Lit Int
    BoolL :: Bool -> Lit Bool

instance A.ToJSON (Lit a) where
    toJSON (StringL t) = A.String t
    toJSON (IntL s) = A.Number $ fromIntegral s
    toJSON (BoolL b) = A.Bool b



--------------------------------------------------------------------------------
-- | Class of values which can be lifted into 'Exp'. All primitive types which
-- exist in both GHC and JavaScript have an instance. This typeclass is used
-- by 'litE'.

class Lift e where
    lift :: e -> Exp e

instance Lift Bool where
    lift = LitE . BoolL

instance Lift Int where
    lift = LitE . IntL

instance Lift Text where
    lift = LitE . StringL



--------------------------------------------------------------------------------

newtype CTag = CTag Text
instance A.ToJSON CTag where
    toJSON (CTag t) = A.String t

data Con0 r = Con0 CTag r
njsCon0 :: Text -> r -> Con0 r
njsCon0 tag v = Con0 (CTag tag) v

data Con1 a r = Con1 CTag (a -> r)
njsCon1 :: Text -> (a -> r) -> Con1 a r
njsCon1 tag f = Con1 (CTag tag) f

data Con2 a b r = Con2 CTag (a -> b -> r)
njsCon2 :: Text -> (a -> b -> r) -> Con2 a b r
njsCon2 tag f = Con2 (CTag tag) f

data Con3 a b c r = Con3 CTag (a -> b -> c -> r)
njsCon3 :: Text -> (a -> b -> c -> r) -> Con3 a b c r
njsCon3 tag f = Con3 (CTag tag) f


data EventHandler a
data RefHandler a



refHandlerE :: Exp (Maybe a) -> Exp (RefHandler a)
refHandlerE = RefHandlerE


derefE :: Int -> Exp a
derefE = DerefE



--------------------------------------------------------------------------------
-- Constructors for the 'Maybe' type.

justE :: Exp a -> Exp (Maybe a)
justE = JustE

nothingE :: Exp (Maybe a)
nothingE = NothingE



litE :: Lift a => a -> Exp a
litE = lift

holeE :: Int -> Exp r
holeE = HoleE

getE :: Exp Text -> Exp o -> Exp r
getE = GetE

invokeE :: Exp Text -> Exp v -> [SomeExp] -> Exp r
invokeE = InvokeE


value0E :: Con0 r -> Exp r
value0E = Value0E

value1E :: Con1 a r -> Exp a -> Exp r
value1E = Value1E

value2E :: Con2 a b r -> Exp a -> Exp b -> Exp r
value2E = Value2E



eventHandlerE :: Exp Bool -> Exp Bool -> Exp Bool -> Exp (Maybe a) -> Exp (EventHandler a)
eventHandlerE = EventHandlerE