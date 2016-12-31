{-# LANGUAGE JavaScriptFFI              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Nauva.NJS.Eval
    ( Context(..)
    , eval
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

import           Control.Monad    (when)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Writer

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

newtype Eval a = Eval { runEval :: ReaderT Context (WriterT [IO ()] (Except ())) a }
    deriving (Functor, Applicative, Monad, MonadError (), MonadWriter [IO ()], MonadReader Context)

eval :: Context -> Exp a -> Either () (JSVal, IO ())
eval ctx e = do
    (a, io) <- runExcept $ runWriterT $ runReaderT (runEval (evalExp e)) ctx
    pure (a, sequence_ io)


evalExpVal :: (FromJSVal r) => Exp a -> Eval r
evalExpVal exp = do
    r <- evalExp exp
    maybe (throwError ()) pure (unsafePerformIO (fromJSVal r))


evalExp :: Exp a -> Eval JSVal

evalExp UnitE = throwError ()

evalExp (HoleE i) = do
    ctx <- ask
    case i of
        0 -> maybe (throwError ()) pure $ ctxArg0 ctx
        1 -> maybe (throwError ()) pure $ ctxArg1 ctx
        _ -> throwError ()

evalExp (LitE (StringL x)) = pure $ unsafePerformIO $ toJSVal x
evalExp (LitE (IntL x)) = pure $ unsafePerformIO $ toJSVal x
evalExp (LitE (BoolL x)) = pure $ unsafePerformIO $ toJSVal x


evalExp GlobalE = pure $ unsafePerformIO js_globalE

evalExp (GetE prop obj) = do
    prop' <- evalExpVal prop
    obj' <- evalExpVal obj

    let val = unsafePerformIO $ js_getE (textToJSString prop') obj'
    maybe (throwError ()) pure (cast val)


evalExp (InvokeE prop obj args) = do
    prop' <- evalExpVal prop
    obj' <- evalExpVal obj

    args' <- forM args $ \(SomeExp arg) ->
        evalExpVal arg

    let val = unsafePerformIO $ js_invokeE (textToJSString prop') obj' ( JSA.fromList args')
    maybe (throwError ()) pure (cast val)

evalExp (Value0E (Con0 (CTag tag) _)) =
    pure $ unsafePerformIO $ do
        t <- toJSVal tag
        toJSVal [t]

evalExp (Value1E (Con1 (CTag tag) _) a) = do
    a' <- evalExpVal a
    pure $ unsafePerformIO $ do
        t <- toJSVal tag
        toJSVal [t, a']

evalExp (Value2E (Con2 (CTag tag) _) a b) = do
    a' <- evalExpVal a
    b' <- evalExpVal b
    pure $ unsafePerformIO $ do
        t <- toJSVal tag
        toJSVal [t, a', b']

evalExp (Value3E _ _ _ _) = error "Value3E"

evalExp NothingE = pure js_null
evalExp (JustE exp) = evalExp exp

evalExp (RefHandlerE exp) = evalExp exp

evalExp (EventHandlerE a b c d) = do
    preventDefault <- evalExpVal a
    stopPropagation <- evalExpVal b
    stopImmediatePropagation <- evalExpVal c
    action <- evalExpVal d

    ev <- evalExp (HoleE 1)

    tell
        [ do
            when preventDefault $ do
                js_preventDefault ev
            when stopPropagation $ do
                js_stopPropagation ev
            when stopImmediatePropagation $ do
                js_stopImmediatePropagation ev
        ]

    pure action


evalExp (DerefE i) = do
    ctx <- ask
    case M.lookup (RefKey i) (ctxRefs ctx) of
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

foreign import javascript unsafe "$1.preventDefault()" js_preventDefault
    :: JSVal -> IO ()

foreign import javascript unsafe "$1.stopPropagation()" js_stopPropagation
    :: JSVal -> IO ()

foreign import javascript unsafe "$1.stopImmediatePropagation()" js_stopImmediatePropagation
    :: JSVal -> IO ()
