{-# LANGUAGE RecordWildCards, DeriveDataTypeable, TupleSections, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-cse #-}
{-# LANGUAGE OverloadedStrings #-}


-- | The application entry point
module Nauvad (main) where

import Control.Exception
import System.IO.Error
import Control.Monad.Extra
import Data.List.Extra
import Data.Maybe
import Data.Tuple.Extra
import Session
import System.Console.CmdArgs
import System.Directory.Extra
import System.Exit
import System.FilePath
import System.IO

import Language.Haskell.Ghcid.Util
import Language.Haskell.Ghcid.Types
import Wait

import Data.Functor
import Prelude

---
import           Data.Monoid
import qualified Data.Aeson            as A
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.String
import           Data.Time.Clock

import qualified Data.ByteString.Char8 as BS8
import           Data.Typeable

import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad.Writer

import           System.Process

import           Network.PortFinder (findPort)
import qualified Network.WebSockets as WS
import           Network.WebSockets.Snap

import qualified Snap.Core           as Snap
import           Snap.Http.Server    (ConfigLog(..), httpServe, setStartupHook, setPort, setAccessLog, setErrorLog)
import           Snap.Util.FileServe (serveDirectory)
import           Snap.Blaze          (blaze)

import           Nauva.Handle
import           Nauva.View

import           Nauva.Product.Nauva.Element.Message (messageEl, MessageProps(..), MessageSeverity(..))
import           Nauva.Product.Nauva.Element.Terminal (terminalEl, TerminalProps(..))

import           Settings


{-

# State Change Triggers (SCT)

- SCT-1: client opens websocket connection
- SCT-2: reading from the client websocket connection fails
- SCT-3: writing to the client websocket connection fails

- SCT-4: reading from the backend websocket connection fails
- SCT-5: writing to the backend websocket connection fails

- SCT-6: ghcid session has started the test command

- SCT-7: ghcid test command has exited

-}

--

data State = State
    { _clientConnection :: Maybe (WS.Connection, ThreadId)
      -- ^ The connection between NauvaD and the client (browser). This
      -- connection is usually created once and is open for the whole nauvad
      -- lifetime.

    , _backendConnection :: Maybe (WS.Connection, ThreadId)
      -- The connection to the backend server. Is killed and re-established
      -- everytime the ghcid session restarts the process.

    , _backendPort :: Maybe Int
      -- The port number allocated for the backend. Everytime the backend
      -- is recompiled and restarted a new port is allocated so that the
      -- backend can immediately bind to it.

    , _sessionOutput :: [Text]
     -- ^ The raw output from the current run (reload) of the GHCi session.
     -- This is what we show in the client.

    , _sessionMessages :: [Load]
      -- ^ These are the internal messages produced by the current session.
      --  If the session was not loaded successfully, these are the messages
      -- which are sent to the client, nicely formatted.
    }


data NauvaD = NauvaD
    { opts :: Options
      -- ^ The commandline options
    , stateVar :: TVar State
      -- ^ The mutable state owned and managed by nauvad itself.
    , session :: Session
      -- ^ The underlying ghci session.
    , waiter :: Waiter
      -- ^ For file watching.
    }


sendToClient :: TVar State -> NVDMessage -> IO ()
sendToClient stateVar msg = do
    mbClientConnection <- _clientConnection <$> atomically (readTVar stateVar)
    case mbClientConnection of
        Nothing -> pure ()
        Just (conn, _) -> (WS.sendPing conn ("ping" :: Text) >> WS.sendTextData conn (A.encode msg)) `catch`
            \(_ :: IOException) -> shutdownClientConnection stateVar -- SCT-3


appendLine :: TVar State -> Text -> IO ()
appendLine stateVar line =
    atomically $ modifyTVar' stateVar $ \s ->
        s { _sessionOutput = _sessionOutput s <> [line] }


registerClientConnection :: TVar State -> (WS.Connection, ThreadId) -> IO ()
registerClientConnection stateVar clientConnection =
    atomically $ modifyTVar' stateVar $ \s ->
        s { _clientConnection = Just clientConnection }


shutdownClientConnection :: TVar State -> IO ()
shutdownClientConnection stateVar = do
    mbClientConnection <- atomically $ do
        state <- readTVar stateVar
        modifyTVar' stateVar $ \s -> s { _clientConnection = Nothing }
        pure $ _clientConnection state

    case mbClientConnection of
        Nothing -> pure ()
        Just (_, threadId) -> killThread threadId


registerBackendConnection :: TVar State -> (WS.Connection, ThreadId) -> IO ()
registerBackendConnection stateVar backendConnection =
    atomically $ modifyTVar' stateVar $ \s ->
        s { _backendConnection = Just backendConnection }


shutdownBackendConnection :: TVar State -> IO ()
shutdownBackendConnection stateVar = do
    mbBackendConnection <- atomically $ do
        state <- readTVar stateVar
        modifyTVar' stateVar $ \s -> s { _backendConnection = Nothing }
        pure $ _backendConnection state

    case mbBackendConnection of
        Nothing -> pure ()
        Just (_, threadId) -> killThread threadId


connectToBackend :: TVar State -> Int -> IO ()
connectToBackend stateVar portNumber = void $ forkIO $ go `catches`
    [ Handler $ \(_ :: IOException) -> do -- SCT-4
        -- Immediately retry to establish the connection to the backend.
        currentBackendPort <- _backendPort <$> atomically (readTVar stateVar)
        if currentBackendPort == Just portNumber
            then connectToBackend stateVar portNumber
            else pure ()

    , Handler $ warnSomeException "connectToBackend"
    ]

  where
    go = WS.runClient "127.0.0.1" portNumber "/_nauva" $ \conn -> do
        shutdownBackendConnection stateVar
        threadId <- myThreadId
        registerBackendConnection stateVar (conn, threadId)

        forever $ do
            datum <- WS.receiveData conn
            case A.eitherDecode datum of
                Right ("spine" :: Text, v :: A.Value, headElements :: A.Value) -> do
                    sendToClient stateVar $ NVDMSpineRaw v headElements

                Right ("location", v, _) -> do
                    sendToClient stateVar $ NVDMLocationRaw v

                _ -> do
                    putStrLn "Failed to decode message"
                    print datum



-- SCT-1
handleClient :: TVar State -> WS.PendingConnection -> IO ()
handleClient stateVar pendingConnection = do
    conn <- WS.acceptRequest pendingConnection
    WS.forkPingThread conn 5

    threadId <- myThreadId
    registerClientConnection stateVar (conn, threadId)

    pump conn `catches`
        [ Handler $ \(_ :: IOException) ->
            shutdownClientConnection stateVar -- SCT-2

        , Handler $ warnSomeException "handleClient/pump"
        ]

  where
    pump conn = forever $ do
        d <- WS.receiveData conn
        sendMessage d

    sendMessage :: Text -> IO ()
    sendMessage d = do
        mbBackendConnection <- _backendConnection <$> atomically (readTVar stateVar)
        case mbBackendConnection of
            Nothing -> pure ()
            Just (conn, _) -> (WS.sendPing conn ("ping" :: Text) >> WS.sendTextData conn d) `catches`
                [ Handler $ \(_ :: IOException) ->
                    shutdownBackendConnection stateVar -- SCT-5

                , Handler $ warnSomeException "handleClient/sendMessage"
                ]

warnSomeException :: String -> SomeException -> IO ()
warnSomeException f (SomeException e) = do
    let rep = typeOf e
        tyCon = typeRepTyCon rep
    putStrLn $ "## " ++ f ++ " Exception: Type " ++ show rep ++ " from module " ++ tyConModule tyCon ++ " from package " ++ tyConPackage tyCon


echo :: TVar State -> Stream -> String -> IO ()
echo stateVar _ s = do
    appendLine stateVar (T.pack s)
    strs <- _sessionOutput <$> atomically (readTVar stateVar)
    spine <- atomically $ do
        (inst, _effects) <- runWriterT $ instantiate (Path []) $ div_
            [style_ rootStyle]
            [terminalEl TerminalProps { terminalLines = strs }]
        toSpine inst

    when (length strs `mod` 10 == 0) (threadDelay 100000)

    sendToClient stateVar $ NVDMSpine spine
    pure ()

  where
    rootStyle = mkStyle $ do
        backgroundColor "#050f14"
        position absolute
        top "0"
        left "0"
        bottom "0"
        right "0"


-- | Command line options
data Options = Options
    {command :: String
    ,arguments :: [String]
    ,test :: [String]
    ,warnings :: Bool
    ,reload :: [FilePath]
    ,restart :: [FilePath]
    ,directory :: FilePath
    }
    deriving (Data,Typeable,Show)

options :: Mode (CmdArgs Options)
options = cmdArgsMode $ Options
    {command = "" &= typ "COMMAND" &= help "Command to run (defaults to ghci or cabal repl)"
    ,arguments = [] &= args &= typ "MODULE"
    ,test = [] &= name "T" &= typ "EXPR" &= help "Command to run after successful loading"
    ,warnings = False &= name "W" &= help "Allow tests to run even with warnings"
    ,restart = [] &= typ "PATH" &= help "Restart the command when the given file or directory contents change (defaults to .ghci and any .cabal file)"
    ,reload = [] &= typ "PATH" &= help "Reload when the given file or directory contents change (defaults to none)"
    ,directory = "." &= typDir &= name "C" &= help "Set the current directory"
    } &= verbosity &=
    program "nauvad" &= summary ("Auto reloading GHCi daemon")


{-
What happens on various command lines:

Hlint with no .ghci file:
- cabal repl - prompt with Language.Haskell.HLint loaded
- cabal exec ghci Sample.hs - prompt with Sample.hs loaded
- ghci - prompt with nothing loaded
- ghci Sample.hs - prompt with Sample.hs loaded
- stack ghci - prompt with all libraries and Main loaded

Hlint with a .ghci file:
- cabal repl - loads everything twice, prompt with Language.Haskell.HLint loaded
- cabal exec ghci Sample.hs - loads everything first, then prompt with Sample.hs loaded
- ghci - prompt with everything
- ghci Sample.hs - loads everything first, then prompt with Sample.hs loaded
- stack ghci - loads everything first, then prompt with libraries and Main loaded

Warnings:
- cabal repl won't pull in any C files (e.g. hoogle)
- cabal exec ghci won't work with modules that import an autogen Paths module

As a result, we prefer to give users full control with a .ghci file, if available
-}
autoOptions :: Options -> IO Options
autoOptions o@Options{..}
    | command /= "" = return $ f [command] []
    | otherwise = do
        files <- getDirectoryContents "."

        -- use unsafePerformIO to get nicer pattern matching for logic (read-only operations)
        let isStack dir = flip catchIOError (const $ return False) $
                doesFileExist (dir </> "stack.yaml") &&^ doesDirectoryExist (dir </> ".stack-work")
        stack <- isStack "." ||^ isStack ".." -- stack file might be parent, see #62

        let cabal = filter ((==) ".cabal" . takeExtension) files
        let opts = ["-fno-code" | null test]
        return $ case () of
            _ | stack ->
                let flags = if null arguments then
                                "stack ghci --test" :
                                ["--no-load" | ".ghci" `elem` files] ++
                                map ("--ghci-options=" ++) opts
                            else
                                "stack exec --test -- ghci" : opts
                in f flags $ "stack.yaml":cabal
              | ".ghci" `elem` files -> f ("ghci":opts) [".ghci"]
              | cabal /= [] -> f (if arguments == [] then "cabal repl":map ("--ghc-options=" ++) opts else "cabal exec -- ghci":opts) cabal
              | otherwise -> f ("ghci":opts) []
    where
        f c r = o{command = unwords $ c ++ map escape arguments, arguments = [], restart = restart ++ r}

        -- in practice we're not expecting many arguments to have anything funky in them
        escape x | ' ' `elem` x = "\"" ++ x ++ "\""
                 | otherwise = x


main :: IO ()
main = do
    -- The internal state of NauvaD: WebSocket connections, backend port,
    -- compiler output etc. This 'TVar' is modified by various parts
    -- and threads of NauvaD.
    stateVar <- newTVarIO State
        { _clientConnection = Nothing
        , _backendConnection = Nothing
        , _backendPort = Nothing
        , _sessionOutput = []
        , _sessionMessages = []
        }

    -- The port of the HTTP/WebSocket server, where clients will connect to.
    -- We immediately create a HTTP server on that port.
    serverPort <- fromIntegral <$> findPort 8000

    -- Start the server in a separate thread. The 'withAsync' function
    -- ensures that the server thread is killed when the application exits.
    withAsync (httpServer serverPort stateVar) $ \_ -> do
        putStrLn ""
        putStrLn ""
        putStrLn (">>> Server running on http://localhost:" <> show serverPort)
        putStrLn ""
        putStrLn ""

        withSession $ \session -> do
            -- On certain Cygwin terminals stdout defaults to BlockBuffering
            hSetBuffering stdout LineBuffering
            hSetBuffering stderr NoBuffering

            opts <- cmdArgsRun options
            withCurrentDirectory (directory opts) $ do
                opts <- autoOptions opts
                opts <- return $ opts{restart = nubOrd $ restart opts, reload = nubOrd $ reload opts}

                withWaiterNotify $ \waiter ->
                    handle (\(UnexpectedExit cmd _) -> putStrLn $ "Command \"" ++ cmd ++ "\" exited unexpectedly") $
                        runGhcid $ NauvaD opts stateVar session waiter


httpServer :: Int -> TVar State -> IO ()
httpServer port stateVar = do
    staticApp <- mkStaticSettings

    httpServe config $ foldl1 (<|>)
        [ Snap.path "_nauva" (runWebSocketsSnap (handleClient stateVar))

          -- Static files required by nauvad itself.
        , staticApp

          -- public dir of the product
        , serveDirectory "../../public"

          -- index.html
        , blaze $ index port
        ]
  where
    config =
        setStartupHook startupHook .
        setPort port .
        setAccessLog (ConfigIoLog BS8.putStrLn) .
        setErrorLog (ConfigIoLog BS8.putStrLn) $
        mempty

    startupHook _ = do
        callProcess "open" ["http://localhost:" <> show port <> "/"]
        pure ()

index :: Int -> H.Html
index port = H.docTypeHtml $ do
    H.head $ do
        H.script $ fromString $ "NAUVA_PORT = " <> show port

    H.body $ do
        H.div H.! A.id "root" $ ""
        H.script H.! A.src "/nauvad.js" $ ""


runGhcid :: NauvaD -> IO ()
runGhcid nd@NauvaD{..} = do
    let Options{..} = opts

    -- Collect the time stamps of files which shall cause us to restart the whole
    -- session (instead of merely reload it).
    restartTimes <- mapM getModTime restart

    fire nd restartTimes (sessionStart session (echo stateVar) command)

-- | Called when the session has finished loading (compiling). The function is given
-- a list of 'Load' messages, and a list of files (modules) that have been loaded.
fire :: NauvaD -> [Maybe UTCTime] -> IO ([Load], [FilePath]) -> IO ()
fire nd@NauvaD{..} restartTimes m = do
    let Options{..} = opts

    nextWait <- waitFiles waiter

    -- Signal to the client that we're (re-)loading the GHCi session, do
    -- the actual work, and once it's done store the output messages in
    -- the state.
    sendToClient stateVar NVDMLoading
    atomically $ modifyTVar' stateVar $ \s -> s { _sessionMessages = [], _sessionOutput = [] }
    (messages, loaded) <- m
    atomically $ modifyTVar' stateVar $ \s -> s { _sessionMessages = messages }

    -- Now we have three options going forward:
    --
    --  1. Loading the session failed completely (eg. due to a syntax error in the cabal file).
    --  2. The session loaded some modules but there were fatal errors or warnings.
    --  3. Everything ok.
    --
    -- In the first two cases, we present the user with a nice error message. In the third case
    -- we can start the dev server and connect to it.

    let (countErrors, countWarnings) = both sum $ unzip [if loadSeverity == Error then (1,0) else (0,1) | m@Message{..} <- messages, loadMessage /= []]
    let renderMessages = countErrors /= 0 || (countWarnings /= 0 && not warnings)

    case (null loaded, renderMessages) of
        -- CASE-1
        (True, _) -> do
            putStrLn "No files loaded, nothing to wait for. Fix the last error and restart."
            reason <- nextWait $ restart ++ reload ++ loaded
            pure ()

        -- CARSE-2
        (_, True) -> do
            let msgs = [m | m@Message{..} <- messages, loadMessage /= []]
            let children = flip map msgs $ \Message{..} ->
                    messageEl MessageProps
                        { filePath = T.pack loadFile
                        , messages = map T.pack loadMessage
                        , severity = case loadSeverity of
                            Warning -> MSWarning
                            Error -> MSError
                        }

            spine <- atomically $ do
                (inst, _effects) <- runWriterT $ instantiate (Path []) $ div_
                    [style_ $ mkStyle (backgroundColor "black" >> overflow "auto" >> padding "2rem" >> position absolute >> top "0" >> left "0" >> bottom "0" >> right "0")]
                    children
                toSpine inst
            sendToClient stateVar $ NVDMSpine spine

            reason <- nextWait $ restart ++ reload ++ loaded
            pure ()

        -- CASE-3
        (_, _) -> do
            sendToClient stateVar NVDMGood

            -- Allocate a fresh port for the backend server.
            backendPort <- fromIntegral <$> findPort 9000
            atomically $ modifyTVar' stateVar $ \s -> s { _backendPort = Just backendPort }

            sessionExecAsync session (intercalate "\n" test ++ " " ++ show backendPort) $ \stderr ->
                hFlush stdout -- may not have been a terminating newline from test output

            -- SCT-6
            connectToBackend stateVar backendPort

            -- The following line will suspend the application until one of the given files change.
            reason <- nextWait $ restart ++ reload ++ loaded

            atomically $ modifyTVar' stateVar $ \s -> s { _backendPort = Nothing }
            shutdownBackendConnection stateVar

    -- When we arrive here it's because one of the files we've been watching
    -- has changed. Determine if we can reload the session or if we have to
    -- do a full restart, and do so.
    restartTimes2 <- mapM getModTime restart
    if restartTimes == restartTimes2 then
        fire nd restartTimes2 (sessionReload session (echo stateVar))
    else
        fire nd restartTimes2 (sessionStart session (echo stateVar) command)
