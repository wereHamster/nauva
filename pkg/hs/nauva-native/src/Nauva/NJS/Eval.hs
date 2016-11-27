{-# LANGUAGE JavaScriptFFI     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}

module Nauva.NJS.Eval
    ( Context(..)
    , evalExp
    , evalExpVal
    ) where


import           Nauva.NJS.Language

import           Data.Either
import           Data.JSString.Text
import           Data.Typeable
import           Data.Monoid
import           Data.Traversable
import           Data.Maybe
import           Data.Map         (Map)
import qualified Data.Map         as M

import           System.IO.Unsafe

import           GHCJS.Types
import           GHCJS.Foreign.Export
import           GHCJS.Marshal

import           JavaScript.Array as JSA

import           Nauva.Internal.Types



data Context = Context
    { ctxRefs :: Map RefKey JSVal
    , ctxArg0 :: Maybe JSVal
    , ctxArg1 :: Maybe JSVal
    }

evalExpVal :: (FromJSVal r) => Context -> Exp a -> Either () r
evalExpVal ctx exp = do
    r <- evalExp ctx exp
    case unsafePerformIO (fromJSVal r) of
        Nothing -> Left ()
        Just x -> Right x



evalExp :: Context -> Exp a -> Either () JSVal

evalExp _ UnitE = error "UnitE"

evalExp ctx (HoleE i) = case i of
    0 -> pure $ fromJust $ ctxArg0 ctx
    1 -> pure $ fromJust $ ctxArg1 ctx

evalExp _ (LitE (StringL x)) = pure $ unsafePerformIO $ toJSVal x
evalExp _ (LitE (IntL x)) = pure $ unsafePerformIO $ toJSVal x
evalExp _ (LitE (BoolL x)) = pure $ unsafePerformIO $ toJSVal x

evalExp _ GlobalE = pure $ unsafePerformIO js_globalE

evalExp ctx (GetE prop obj) = do
    prop' <- evalExpVal ctx prop
    obj' <- evalExpVal ctx obj

    unsafePerformIO $ do
        val <- js_getE (textToJSString prop') obj'
        case cast val of
            Nothing -> pure $ Left ()
            Just r -> pure $ Right r 


evalExp ctx (InvokeE prop obj args) = do
    prop' <- evalExpVal ctx prop
    obj' <- evalExpVal ctx obj

    args' <- forM args $ \(SomeExp arg) ->
        evalExpVal ctx arg

    unsafePerformIO $ do
        let args'' = JSA.fromList args'
        val <- js_invokeE (textToJSString prop') obj' args''
        case cast val of
            Nothing -> pure $ Left ()
            Just r -> pure $ Right r 

evalExp ctx (Value0E (Con0 (CTag tag) _)) =
    pure $ unsafePerformIO $ do
        t <- toJSVal tag
        toJSVal [t]

evalExp ctx (Value1E (Con1 (CTag tag) _) a) = do
    a' <- evalExpVal ctx a
    pure $ unsafePerformIO $ do
        t <- toJSVal tag
        toJSVal [t, a']

evalExp ctx (Value2E (Con2 (CTag tag) _) a b) = do
    a' <- evalExpVal ctx a
    b' <- evalExpVal ctx b
    pure $ unsafePerformIO $ do
        t <- toJSVal tag
        toJSVal [t, a', b']

evalExp ctx (Value3E _ _ _ _) = error "Value3E"

evalExp ctx NothingE = pure js_null
evalExp ctx (JustE exp) = evalExp ctx exp

evalExp ctx (RefHandlerE exp) = evalExp ctx exp

evalExp ctx (EventHandlerE a b c d) = do
    -- preventDefault <- evalExpVal ctx a
    -- stopPropagation <- evalExpVal ctx b
    -- stopImmediatePropagation <- evalExpVal ctx c
    action <- evalExpVal ctx d
    
    pure action

evalExp ctx (DerefE i) = case M.lookup (RefKey i) (ctxRefs ctx) of
    Nothing -> error "DerefE"
    Just x  -> pure x


foreign import javascript unsafe "$2[$1]" js_getE
    :: JSString -> JSVal -> IO JSVal

foreign import javascript unsafe "$2[$1].apply($2, $3)" js_invokeE
    :: JSString -> JSVal -> JSArray -> IO JSVal

foreign import javascript unsafe "$r = window" js_globalE
    :: IO JSVal

foreign import javascript unsafe "$r = null" js_null
    :: JSVal
