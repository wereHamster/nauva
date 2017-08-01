{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE JavaScriptFFI              #-}
{-# LANGUAGE OverloadedStrings          #-}

module Nauva.NJS.Eval
    ( Context(..)
    , eval
    ) where


import           Nauva.NJS

import           Data.Either
import           Data.JSString.Text
import qualified Data.JSString    as JSS
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

import           JavaScript.Array           as JSA
import           JavaScript.Array.Internal  (fromList)
import qualified JavaScript.Object          as O
import qualified JavaScript.Object.Internal as O

import           Nauva.Internal.Types



data Context = Context
    { ctxRefs :: Map RefKey JSVal
    , ctxArgs :: [JSVal]
    }

eval :: Context -> F a -> Either () (JSVal, IO ())
eval ctx f = do
    let refs = unsafePerformIO $ do
            o <- O.create
            forM_ (M.toList (ctxRefs ctx)) $ \((RefKey k), val) -> do
                O.setProp (JSS.pack $ show k) val o
            pure $ jsval o

    let args = jsval $ fromList $ ctxArgs ctx

    let jsf = unsafePerformIO $ do
            o <- O.create
            O.setProp "constructors" (jsval $ fromList $ map (jsval . textToJSString) $ fConstructors f) o
            O.setProp "arguments" (jsval $ fromList $ map (jsval . textToJSString) $ fArguments f) o
            O.setProp "body" (jsval $ textToJSString $ fBody f) o
            pure $ jsval o

    let r = unsafePerformIO $ js_evalF refs args jsf
    Right (r, pure ())


foreign import javascript unsafe "evalF($1, $2, $3)" js_evalF
    :: JSVal -> JSVal -> JSVal -> IO JSVal