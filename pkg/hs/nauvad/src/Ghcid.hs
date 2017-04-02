{-# LANGUAGE RecordWildCards, DeriveDataTypeable, TupleSections, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-cse #-}
{-# LANGUAGE OverloadedStrings #-}


-- | The application entry point
module Ghcid(main, mainWithTerminal) where

import Control.Exception
import System.IO.Error
import Control.Monad.Extra
import Data.List.Extra
import Data.Maybe
import Data.Tuple.Extra
import Session
import qualified System.Console.Terminal.Size as Term
import System.Console.CmdArgs
import System.Console.ANSI
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

import qualified Data.ByteString.Char8 as BS8
import           Data.Typeable

import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM

import           System.Process

import           Network.PortFinder (findPort)
import qualified Network.WebSockets as WS
import           Network.WebSockets.Snap

import qualified Snap.Core           as Snap
import           Snap.Http.Server    (ConfigLog(..), httpServe, setStartupHook, setPort, setAccessLog, setErrorLog)
import           Snap.Util.FileServe (serveDirectory)
import           Snap.Blaze          (blaze)

import           Nauva.Handle
import           Nauva.View hiding (Style, width, height)

import           Nauva.Product.Nauva.Element.Message (messageEl, MessageProps(..), MessageSeverity(..))

import           Settings
---

-- | Command line options
data Options = Options
    {command :: String
    ,arguments :: [String]
    ,test :: [String]
    ,warnings :: Bool
    ,no_status :: Bool
    ,height :: Maybe Int
    ,width :: Maybe Int
    ,topmost :: Bool
    ,no_title :: Bool
    ,project :: String
    ,reload :: [FilePath]
    ,restart :: [FilePath]
    ,directory :: FilePath
    ,outputfile :: [FilePath]
    }
    deriving (Data,Typeable,Show)

options :: Mode (CmdArgs Options)
options = cmdArgsMode $ Options
    {command = "" &= typ "COMMAND" &= help "Command to run (defaults to ghci or cabal repl)"
    ,arguments = [] &= args &= typ "MODULE"
    ,test = [] &= name "T" &= typ "EXPR" &= help "Command to run after successful loading"
    ,warnings = False &= name "W" &= help "Allow tests to run even with warnings"
    ,no_status = False &= name "S" &= help "Suppress status messages"
    ,height = Nothing &= help "Number of lines to use (defaults to console height)"
    ,width = Nothing &= name "w" &= help "Number of columns to use (defaults to console width)"
    ,topmost = False &= name "t" &= help "Set window topmost (Windows only)"
    ,no_title = False &= help "Don't update the shell title/icon"
    ,project = "" &= typ "NAME" &= help "Name of the project, defaults to current directory"
    ,restart = [] &= typ "PATH" &= help "Restart the command when the given file or directory contents change (defaults to .ghci and any .cabal file)"
    ,reload = [] &= typ "PATH" &= help "Reload when the given file or directory contents change (defaults to none)"
    ,directory = "." &= typDir &= name "C" &= help "Set the current directory"
    ,outputfile = [] &= typFile &= name "o" &= help "File to write the full output to"
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


-- | Like 'main', but run with a fake terminal for testing
mainWithTerminal :: IO (Int,Int) -> ([(Style,String)] -> IO ()) -> IO ()
mainWithTerminal termSize termOutput = withSession $ \session -> do
    -- On certain Cygwin terminals stdout defaults to BlockBuffering
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr NoBuffering
    opts <- cmdArgsRun options
    withCurrentDirectory (directory opts) $ do
        opts <- autoOptions opts
        opts <- return $ opts{restart = nubOrd $ restart opts, reload = nubOrd $ reload opts}

        termSize <- return $ case (width opts, height opts) of
            (Just w, Just h) -> return (w,h)
            (w, h) -> do
                term <- termSize
                -- if we write to the final column of the window then it wraps automatically
                -- so putStrLn width 'x' uses up two lines
                return (fromMaybe (pred $ fst term) w, fromMaybe (snd term) h)

        withWaiterNotify $ \waiter ->
            handle (\(UnexpectedExit cmd _) -> putStrLn $ "Command \"" ++ cmd ++ "\" exited unexpectedly") $
                runGhcid session waiter termSize termOutput opts



main :: IO ()
main = mainWithTerminal termSize termOutput
    where
        termSize = maybe (80, 8) (Term.width &&& Term.height) <$> Term.size

        termOutput xs = do
            pure ()
            -- outWith $ forM_ (groupOn fst xs) $ \x@((s,_):_) -> do
            --     when (s == Bold) $ setSGR [SetConsoleIntensity BoldIntensity]
            --     putStr $ concatMap ((:) '\n' . snd) x
            --     when (s == Bold) $ setSGR []
            -- hFlush stdout -- must flush, since we don't finish with a newline


data Style = Plain | Bold deriving Eq

server :: Int -> TChan NVDMessage -> TMVar WS.Connection -> IO ()
server port chan connTMVar = do
    let startupHook _ = do
            callProcess "open" ["http://localhost:" <> show port <> "/"]
            pure ()

    let config = setStartupHook startupHook .
                 setPort port .
                 setAccessLog (ConfigIoLog BS8.putStrLn) .
                 setErrorLog (ConfigIoLog BS8.putStrLn) $
                 mempty

    staticApp <- mkStaticSettings

    httpServe config $ foldl1 (<|>)
        [ Snap.path "_nauva" (runWebSocketsSnap (websocketApplication chan connTMVar))

          -- Static files required by nauvad itself.
        , staticApp

          -- public dir of the product
        , serveDirectory "../../public"

          -- index.html
        , blaze $ index port
        ]

websocketApplication :: TChan NVDMessage -> TMVar WS.Connection -> WS.PendingConnection -> IO ()
websocketApplication chan connTMVar pendingConnection = do
    conn <- WS.acceptRequest pendingConnection
    WS.forkPingThread conn 5

    chanCopy <- atomically $ dupTChan chan
    void $ forkIO $ forever $ do
        msg <- atomically $ readTChan chanCopy
        WS.sendTextData conn $ A.encode msg

    WS.sendTextData conn $ A.encode NVDMLoading

    -- Forever read messages from the WebSocket and process.
    forever $ do
        d <- WS.receiveData conn
        -- print $ "recv: " <> d
        mbOutConn <- atomically $ tryReadTMVar connTMVar
        case mbOutConn of
            Nothing -> do
                -- print "no out connection"
                pure ()
            Just outConn -> do
                -- print $ "out: " <> d
                WS.sendTextData outConn (d :: Text) `catch` \(e :: WS.ConnectionException) -> pure ()


index :: Int -> H.Html
index port = H.docTypeHtml $ do
    H.head $ do
        H.script H.! A.src "/react.min.js" $ ""
        H.script H.! A.src "/react-dom.min.js" $ ""

    H.body $ do
        H.div H.! A.id "root" $ ""

        H.script $ fromString $ "NAUVA_PORT = " <> show port
        H.script H.! A.src "/nauva-dev-server.js" $ ""


runGhcid :: Session -> Waiter -> IO (Int,Int) -> ([(Style,String)] -> IO ()) -> Options -> IO ()
runGhcid session waiter termSize termOutput opts@Options{..} = do
    port <- fromIntegral <$> findPort 8000

    putStrLn ""
    putStrLn ""
    putStrLn (">>> Server running on http://localhost:" <> show port)
    putStrLn ""
    putStrLn ""

    chan <- newTChanIO
    connTMVar <- newEmptyTMVarIO
    _ <- forkIO $ server port chan connTMVar

    echoTVar <- newTVarIO []

    let echo _ s = do
            strs <- atomically $ do
                modifyTVar' echoTVar (\x -> x <> [T.pack s])
                readTVar echoTVar

            let els = (flip map) strs $ \str -> div_ [style_ $ mkStyle (whiteSpace "nowrap" >> overflow "hidden" >> cssTerm "text-overflow" "ellipsis")] [str_ str]

            spine <- atomically $ do
                (inst, _effects) <- instantiate (Path []) (div_ [style_ $ mkStyle (fontSize "12px" >> lineHeight "16px" >> fontFamily "'SFMono-Regular', Consolas, 'Liberation Mono', Menlo, Courier, monospace" >> backgroundColor "black" >> color "white" >> overflow "auto" >> padding "2rem" >> position absolute >> top "0" >> left "0" >> bottom "0" >> right "0")] els)
                toSpine inst
            atomically $ writeTChan chan $ NVDMSpine spine


    let outputFill :: Maybe (Int, [Load]) -> [String] -> IO ()
        outputFill load msg = do
            (width, height) <- termSize
            let n = height - length msg
            load <- return $ take (if isJust load then n else 0) $ prettyOutput (maybe 0 fst load)
                [ m{loadMessage = concatMap (chunksOfWord width (width `div` 5)) $ loadMessage m}
                | m@Message{} <- maybe [] snd load]
            termOutput $ load ++ map (Plain,) msg ++ replicate (height - (length load + length msg)) (Plain,"")

    restartTimes <- mapM getModTime restart
    curdir <- getCurrentDirectory

    -- fire, given a waiter, the messages/loaded
    let fire nextWait (messages, loaded) = do
            let loadedCount = length loaded
            whenLoud $ do
                outStrLn $ "%MESSAGES: " ++ show messages
                outStrLn $ "%LOADED: " ++ show loaded

            let (countErrors, countWarnings) = both sum $ unzip
                    [if loadSeverity == Error then (1,0) else (0,1) | m@Message{..} <- messages, loadMessage /= []]
            test <- return $
                if null test || countErrors /= 0 || (countWarnings /= 0 && not warnings) then Nothing
                else Just $ intercalate "\n" test


            case (countErrors, countWarnings) of
                (0, _) -> do
                    atomically $ do writeTChan chan NVDMGood
                    -- spine <- atomically $ do
                    --     inst <- instantiate (div_ [str_ "good"])
                    --     toSpine inst
                    -- atomically $ writeTChan chan $ NVDMSpine spine

                    -- let msgs = [m | m@Message{..} <- messages, loadMessage /= []]
                    -- let children = (flip map) msgs $ \Message{..} ->
                    --         messageEl $ MessageProps
                    --             { filePath = T.pack loadFile
                    --             , messages = map T.pack loadMessage
                    --             }

                    -- spine <- atomically $ do
                    --     inst <- instantiate (div_ [style_ $ mkStyle (backgroundColor "black" >> overflow "auto" >> padding "2rem" >> position absolute >> top "0" >> left "0" >> bottom "0" >> right "0")] children)
                    --     toSpine inst
                    -- atomically $ writeTChan chan $ NVDMSpine spine
                    pure ()

                _ -> do
                    let msgs = [m | m@Message{..} <- messages, loadMessage /= []]
                    let children = (flip map) msgs $ \Message{..} ->
                            messageEl $ MessageProps
                                { filePath = T.pack loadFile
                                , messages = map T.pack loadMessage
                                , severity = case loadSeverity of
                                    Warning -> MSWarning
                                    Error -> MSError
                                }

                    spine <- atomically $ do
                        (inst, _effects) <- instantiate (Path []) (div_ [style_ $ mkStyle (backgroundColor "black" >> overflow "auto" >> padding "2rem" >> position absolute >> top "0" >> left "0" >> bottom "0" >> right "0")] children)
                        toSpine inst
                    atomically $ writeTChan chan $ NVDMSpine spine
                    pure ()

            let updateTitle extra = unless no_title $ setTitle $
                    let f n msg = if n == 0 then "" else show n ++ " " ++ msg ++ ['s' | n > 1]
                    in (if countErrors == 0 && countWarnings == 0 then allGoodMessage else f countErrors "error" ++
                       (if countErrors >  0 && countWarnings >  0 then ", " else "") ++ f countWarnings "warning") ++
                       " " ++ extra ++ [' ' | extra /= ""] ++ "- " ++
                       (if null project then takeFileName curdir else project)

            updateTitle $ if isJust test then "(running test)" else ""
            outputFill (Just (loadedCount, messages)) ["Running test..." | isJust test]

            appPort <- fromIntegral <$> findPort 9000

            wsClientThreadIdTMVar <- newEmptyTMVarIO
            let runClient :: IO ()
                runClient = do
                    -- print "runClient"
                    mbConn <- atomically $ tryTakeTMVar connTMVar
                    case mbConn of
                        Nothing -> pure ()
                        Just conn -> do
                            WS.sendClose conn ("Bye!" :: Text) `catch` \(e :: SomeException) -> do
                                pure ()

                            let drain = WS.receiveDataMessage conn >> drain
                            drain `catch`  \(e :: SomeException) -> do
                                pure ()

                    void $ forkIO $ do
                        mbOldThreadId <- atomically $ tryTakeTMVar wsClientThreadIdTMVar
                        case mbOldThreadId of
                            Nothing -> pure ()
                            Just tId -> killThread tId

                        threadId <- myThreadId
                        atomically $ putTMVar wsClientThreadIdTMVar threadId

                        -- print $ "run WS Client -> " ++ show appPort

                        let go = WS.runClient "localhost" appPort "/_nauva" $ \conn -> do
                                -- print "Registering out conn"
                                atomically $ putTMVar connTMVar conn
                                forever $ do
                                    datum <- WS.receiveData conn
                                    case A.eitherDecode datum of
                                        Left e -> do
                                            putStrLn "Failed to decode message"
                                            print datum
                                            print e

                                        Right ("spine" :: Text, v :: A.Value) -> do
                                            atomically $ writeTChan chan $ NVDMSpineRaw v
                                            pure ()

                                        Right ("location", v) -> do
                                            atomically $ writeTChan chan $ NVDMLocationRaw v
                                            pure ()

                                        _ -> do
                                            putStrLn "Failed to decode message"
                                            print datum
                                    pure ()

                        go `catches`
                            [ Handler $ \(e :: IOException) -> do
                                -- print e
                                runClient

                            , Handler $ \(SomeException e) -> do
                                -- print "catch in go"
                                let rep = typeOf e
                                    tyCon = typeRepTyCon rep
                                putStrLn $ "## Exception: Type " ++ show rep ++ " from module " ++ tyConModule tyCon ++ " from package " ++ tyConPackage tyCon
                            ]

                        pure ()

            runClient

            forM_ outputfile $ \file ->
                writeFile file $ unlines $ map snd $ prettyOutput loadedCount $ filter isMessage messages
            when (null loaded) $ do
                putStrLn $ "No files loaded, nothing to wait for. Fix the last error and restart."
                exitFailure
            whenJust test $ \t -> do
                whenLoud $ outStrLn $ "%TESTING: " ++ t
                sessionExecAsync session (t ++ " " ++ (show appPort)) $ \stderr -> do
                    whenLoud $ outStrLn "%TESTING: Completed"
                    hFlush stdout -- may not have been a terminating newline from test output
                    if "*** Exception: " `isPrefixOf` stderr then do
                        updateTitle "(test failed)"
                     else
                        updateTitle "(test done)"

            reason <- nextWait $ restart ++ reload ++ loaded

            -- One or more files have been modified. Reload the session or restart GHCi.

            unless no_status $ outputFill Nothing $ "Reloading..." : map ("  " ++) reason
            atomically $ do writeTChan chan NVDMLoading
            restartTimes2 <- mapM getModTime restart
            if restartTimes == restartTimes2 then do
                nextWait <- waitFiles waiter
                fire nextWait =<< sessionReload session echo
            else do
                runGhcid session waiter termSize termOutput opts

    nextWait <- waitFiles waiter
    (messages, loaded) <- sessionStart session echo command
    when (null loaded) $ do
        putStrLn $ "\nNo files loaded, GHCi is not working properly.\nCommand: " ++ command
        exitFailure
    fire nextWait (messages, loaded)


-- | Given an available height, and a set of messages to display, show them as best you can.
prettyOutput :: Int -> [Load] -> [(Style,String)]
prettyOutput loaded [] = [(Plain,allGoodMessage ++ " (" ++ show loaded ++ " module" ++ ['s' | loaded /= 1] ++ ")")]
prettyOutput loaded xs = concat $ msg1:msgs
    where (err, warn) = partition ((==) Error . loadSeverity) xs
          msg1:msgs = map (map (Bold,) . loadMessage) err ++ map (map (Plain,) . loadMessage) warn
