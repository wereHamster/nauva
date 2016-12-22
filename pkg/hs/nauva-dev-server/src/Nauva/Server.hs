{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Nauva.Server
    ( Config(..)
    , runServer
    ) where


import           Data.Default
import           Data.Text             (Text)
import qualified Data.Aeson            as A
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy  as LBS
import           Data.Monoid
import           Data.String

import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A
import qualified Text.Blaze.Html.Renderer.Utf8  as H

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad

import           System.Directory

import           Nauva.Handle
import           Nauva.Internal.Types

import qualified Network.WebSockets as WS
import           Network.WebSockets.Snap

import           Snap.Core           (path, pass)
import           Snap.Http.Server    (ConfigLog(..), httpServe, setPort, setAccessLog, setErrorLog)
import           Snap.Util.FileServe (serveDirectory)
import           Snap.Blaze          (blaze)

import           Nauva.Server.Settings (mkStaticSettings)

import           Prelude



data Config = Config
    { cPort :: Int
      -- ^ The HTTP port on which the server will listen and accept WebSocket
      -- connections.

    , cElement :: Element
      -- ^ The root elment of the application. This will be rendered into the
      -- Handle once.

    , cPublicDir :: Maybe String
      -- ^ If present, files from this directory will be made available to the
      -- client. These files are served /after/ the stuff that is needed by
      -- nauva.

    , cHead :: H.Html
      -- ^ Extra stuff to inject into the HTML <head>. It is added /after/
      -- the code that is needed by nauva. In particular, it means that
      -- React and ReactDOM are available to scripts loaded here.
    }


runServer :: Config -> IO ()
runServer c = do
    nauvaH <- newHandle
    render nauvaH (cElement c)

    -- Try to restore the application from the snapshot.
    stateExists <- doesFileExist "snapshot.json"
    when stateExists $ do
        f <- LBS.readFile "snapshot.json"
        case A.decode f of
            Nothing -> pure ()
            Just v -> restoreSnapshot nauvaH v

    -- Persist the state after each change.
    changeSignalCopy <- atomically $ dupTChan (changeSignal nauvaH)
    void $ forkIO $ forever $ do
        m <- atomically $ do
            void $ readTChan changeSignalCopy
            createSnapshot nauvaH
        LBS.writeFile "snapshot.json" (A.encode m)


    staticApp <- mkStaticSettings

    let config = setPort (cPort c) . setAccessLog (ConfigIoLog BS8.putStrLn) . setErrorLog (ConfigIoLog BS8.putStrLn) $ mempty
    httpServe config $ foldl1 (<|>)
        [ path "ws" (runWebSocketsSnap (websocketApplication nauvaH))
        , staticApp

        , case cPublicDir c of
            Nothing -> pass
            Just publicDir -> serveDirectory publicDir

        , blaze $ index $ cHead c
        ]


websocketApplication :: Handle -> WS.PendingConnection -> IO ()
websocketApplication nauvaH pendingConnection = do
    conn <- WS.acceptRequest pendingConnection
    WS.forkPingThread conn 30

    -- Fork a thread to the background and send the spine to the client
    -- whenever the root instance in the 'Handle' changes.
    changeSignalCopy <- atomically $ dupTChan (changeSignal nauvaH)
    void $ forkIO $ forever $ do
        spine <- atomically $ do
            void $ readTChan changeSignalCopy
            rootInstance <- readTMVar (hInstance nauvaH)
            toSpine rootInstance

        WS.sendTextData conn $ A.encode spine

    -- Send the current state of the application.
    spine <- atomically $ do
        rootInstance <- readTMVar (hInstance nauvaH)
        toSpine rootInstance
    WS.sendTextData conn $ A.encode spine

    -- Forever read messages from the WebSocket and process.
    forever $ do
        datum <- WS.receiveData conn
        case A.eitherDecode datum of
            Left e -> do
                putStrLn "Failed to decode message"
                print datum
                print e

            Right msg -> do
                case msg of
                    (HookM path value) -> do
                        void $ dispatchHook nauvaH path value

                    (ActionM path _ value) -> do
                        void $ dispatchEvent nauvaH path value

                    (RefM path value) -> do
                        void $ dispatchRef nauvaH path value


data Message
    = HookM Path A.Value
    | ActionM Path Text A.Value
    | RefM Path A.Value

instance A.FromJSON Message where
    parseJSON v = do
        list <- A.parseJSON v
        case list of
            (A.String "hook"):path:value:[] -> HookM
                <$> A.parseJSON path
                <*> A.parseJSON value

            (A.String "action"):path:name:value:[] -> ActionM
                <$> A.parseJSON path
                <*> A.parseJSON name
                <*> A.parseJSON value

            (A.String "ref"):path:value:[] -> RefM
                <$> A.parseJSON path
                <*> A.parseJSON value

            _ -> fail "Message"


index :: H.Html -> H.Html
index headExtras = H.docTypeHtml $ do
    H.head $ do
        H.title "Nauva Dev Server"

        H.script H.! A.src "/react.min.js" $ ""
        H.script H.! A.src "/react-dom.min.js" $ ""

        H.link H.! A.rel "stylesheet" H.! A.type_ "text/css" H.! A.href "/nauva-dev-server.css"

        headExtras

    H.body $ do
        H.div H.! A.id "root" $ ""
        H.script H.! A.src "/nauva-dev-server.js" $ ""
