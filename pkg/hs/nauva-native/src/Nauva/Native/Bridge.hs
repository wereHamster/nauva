{-# LANGUAGE JavaScriptFFI     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Nauva.Native.Bridge
    ( Impl(..)
    , Callbacks(..)

    , DOMElement
    , getElementById

    , Bridge(..)
    , newBridge
    , pushLocation
    , renderHead
    , renderSpine
    , renderSpineAtPath
    ) where


import           Data.JSString (JSString, pack)
import           Data.Text     (Text)
import qualified Data.Text     as T
import           Data.List

import           Control.Monad


import           GHCJS.Types
import           GHCJS.Foreign
import           GHCJS.Marshal
import           GHCJS.Foreign.Callback

import qualified JavaScript.Object          as O
import qualified JavaScript.Object.Internal as O

import           Nauva.Internal.Types
import           Nauva.NJS

import           Unsafe.Coerce


-- A wrapper around a pointer to a native DOM element.
newtype DOMElement = DOMElement { unDOMElement :: JSVal }

foreign import javascript unsafe "document.getElementById($1)" js_getElementById
    :: JSString -> IO DOMElement

getElementById :: JSString -> IO DOMElement
getElementById = js_getElementById



data Impl = Impl
    { sendLocationImpl :: Text -> IO ()
    , componentEventImpl :: Path -> FID -> JSVal -> IO ()
    , nodeEventImpl :: Path -> FID -> JSVal -> IO ()
    , attachRefImpl :: Path -> JSVal -> IO ()
    , detachRefImpl :: Path -> IO ()
    , componentDidMountImpl :: Path -> IO ()
    , componentWillUnmountImpl :: Path -> IO ()
    }



-------------------------------------------------------------------------------
-- A 'Peer' is a native JavaScript object which implements the necessary
-- interface so that the bridge can communicate with the DOM.

data Callbacks = Callbacks
    { sendLocationCallback :: Callback (JSVal -> IO ())
    , componentEventCallback :: Callback (JSVal -> JSVal -> JSVal -> IO ())
    , nodeEventCallback :: Callback (JSVal -> JSVal -> JSVal -> IO ())
    , attachRefCallback :: Callback (JSVal -> JSVal -> IO ())
    , detachRefCallback :: Callback (JSVal -> IO ())
    , componentDidMountCallback :: Callback (JSVal -> IO ())
    , componentWillUnmountCallback :: Callback (JSVal -> IO ())
    }

instance ToJSVal Callbacks where
    toJSVal cb = do
        o <- O.create

        O.setProp "sendLocation" (unsafeCoerce $ sendLocationCallback cb) o
        O.setProp "componentEvent" (unsafeCoerce $ componentEventCallback cb) o
        O.setProp "nodeEvent" (unsafeCoerce $ nodeEventCallback cb) o
        O.setProp "attachRef" (unsafeCoerce $ attachRefCallback cb) o
        O.setProp "detachRef" (unsafeCoerce $ detachRefCallback cb) o
        O.setProp "componentDidMount" (unsafeCoerce $ componentDidMountCallback cb) o
        O.setProp "componentWillUnmount" (unsafeCoerce $ componentWillUnmountCallback cb) o

        pure $ jsval o


-------------------------------------------------------------------------------
-- | The 'Bridge' is an opaque JSVal representing the 'Bridge's counter-part
-- in JavaScript.

newtype Bridge = Bridge { unBridge :: JSVal }


foreign import javascript unsafe "newBridge($1, $2)" js_newBridge
    :: DOMElement -> JSVal -> IO Bridge

foreign import javascript unsafe "$1.pushLocation($2)" js_pushLocation
    :: Bridge -> JSVal -> IO ()

foreign import javascript unsafe "$1.renderHead($2)" js_renderHead
    :: Bridge -> JSVal -> IO ()

foreign import javascript unsafe "$1.renderSpine($2)" js_renderSpine
    :: Bridge -> JSVal -> IO ()

foreign import javascript unsafe "$1.renderSpineAtPath($2, $3)" js_renderSpineAtPath
    :: Bridge -> JSString -> JSVal -> IO ()




instance FromJSVal Path where
    fromJSVal v = case jsonTypeOf v of
        JSONArray -> do
            vals <- fromJSValUnchecked v
            keys <- forM vals $ \k -> case jsonTypeOf k of
                JSONString  -> KString <$> fromJSValUnchecked k
                JSONInteger -> KIndex <$> fromJSValUnchecked k
                _           -> Prelude.error "..."

            pure $ Just $ Path keys



--------------------------------------------------------------------------------
-- | Initialize the native JavaScript code so that you can render instances
-- into the given DOM element (container). The 'Impl' contains the implementation
-- of event, ref and other handlers.
--
-- See 'renderSpine' and 'renderSpineAtPath'.

newBridge :: DOMElement -> Impl -> IO Bridge
newBridge el impl = do
    sendLocationCallback <- syncCallback1 ContinueAsync $ \vPath -> do
        path <- fromJSValUnchecked vPath
        sendLocationImpl impl path

    componentEventCallback <- syncCallback3 ContinueAsync $ \vPath vFID vEvent -> do
        path <- fromJSValUnchecked vPath
        fid <- FID <$> fromJSValUnchecked vFID
        componentEventImpl impl path fid vEvent

    nodeEventCallback <- syncCallback3 ContinueAsync $ \vPath vFID vEvent -> do
        path <- fromJSValUnchecked vPath
        fid <- FID <$> fromJSValUnchecked vFID
        nodeEventImpl impl path fid vEvent

    attachRefCallback <- syncCallback2 ContinueAsync $ \vPath vRef -> do
        path <- fromJSValUnchecked vPath
        attachRefImpl impl path vRef

    detachRefCallback <- syncCallback1 ContinueAsync $ \vPath -> do
        path <- fromJSValUnchecked vPath
        detachRefImpl impl path

    componentDidMountCallback <- syncCallback1 ContinueAsync $ \vPath -> do
        path <- fromJSValUnchecked vPath
        componentDidMountImpl impl path

    componentWillUnmountCallback <- syncCallback1 ContinueAsync $ \vPath -> do
        path <- fromJSValUnchecked vPath
        componentWillUnmountImpl impl path


    callbacks <- toJSVal $ Callbacks
        { sendLocationCallback         = sendLocationCallback
        , componentEventCallback       = componentEventCallback
        , nodeEventCallback            = nodeEventCallback
        , attachRefCallback            = attachRefCallback
        , detachRefCallback            = detachRefCallback
        , componentDidMountCallback    = componentDidMountCallback
        , componentWillUnmountCallback = componentWillUnmountCallback
        }

    js_newBridge el callbacks


pushLocation :: Bridge -> JSVal -> IO ()
pushLocation = js_pushLocation

renderHead :: Bridge -> JSVal -> IO ()
renderHead = js_renderHead

renderSpine :: Bridge -> JSVal -> IO ()
renderSpine = js_renderSpine

renderSpineAtPath :: Bridge -> [Key] -> JSVal -> IO ()
renderSpineAtPath bridge path = js_renderSpineAtPath bridge key
  where
    key = pack $ intercalate "." $ map toStr path
    toStr (KIndex  i) = show i
    toStr (KString t) = T.unpack t
