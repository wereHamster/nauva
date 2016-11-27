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
import qualified Data.ByteString.Lazy  as LBS

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad

import           System.Directory

import           Nauva.Handle
import           Nauva.Internal.Types

import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WebSockets (websocketsOr)
import           Network.Wai.Application.Static
import           Network.Wai.Middleware.RequestLogger

import qualified Network.WebSockets as WS


-- The actual module that is imported is selected in the cabal file,
-- depending on flags. It contains a function which generates the settings
-- for 'staticApp'. We have two modes:
--
--  - Embed files directly into the binary (by use of file-embed). This
--    is useful when building the production build of the binary. It makes
--    deployment easier (single binary without any external dependencies,
--    well, beyond the GHC runtime). But it doesn't work well with GHCi,
--    as GHCi won't recompile the Haskell module when any of the embedded
--    files change.
--
--  - Load the files from a directory on the filesystem. This is useful
--    when you are developing nauva-dev-server and are frequently editing
--    those files.

import           Nauva.Server.Settings (mkStaticSettings)



data Config = Config
    { cPort :: Int
      -- ^ The HTTP port on which the server will listen and accept WebSocket
      -- connections.
    , cElement :: Element
      -- ^ The root elment of the application. This will be rendered into the
      -- Handle once.
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


    logger <- mkRequestLogger def
    staticSettings <- mkStaticSettings
    run (cPort c) $ logger $ websocketsOr WS.defaultConnectionOptions
        (websocketApplication nauvaH)
        (staticApp staticSettings)


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
