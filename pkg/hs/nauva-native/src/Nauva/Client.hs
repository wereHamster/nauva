{-# LANGUAGE JavaScriptFFI     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Nauva.Client
    ( runClient
    ) where


import qualified Data.Text               as T
import qualified Data.Aeson              as A
import qualified Data.Aeson.Types        as A
import           Data.Monoid
import           Data.Maybe

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Writer

import           System.IO.Unsafe

import           Nauva.App
import           Nauva.Handle
import           Nauva.Internal.Types
import           Nauva.View

import           Nauva.Native.Bridge

import           GHCJS.Types
import           GHCJS.Marshal
import           Data.JSString.Text
import qualified Data.JSString              as JSS

import qualified JavaScript.Object          as O
import           JavaScript.Array.Internal  (fromList)



newHeadH :: Handle -> Bridge -> IO HeadH
newHeadH nauvaH bridge = do
    var <- newTVarIO []
    pure $ HeadH
        { hElements = var
        , hReplace = \newElements -> do
            print (length newElements)
            atomically $ writeTVar var newElements
            processSignals nauvaH

            h <- atomically $ do
                instances <- mapM (\x -> fst <$> runWriterT (instantiate (Path []) x)) newElements
                mapM instanceToJSVal instances

            renderHead bridge (jsval $ fromList h)
        }


newRouterH :: Handle -> IO RouterH
newRouterH nauvaH = do
    var <- newTVarIO (Location "/")
    chan <- newTChanIO

    pure $ RouterH
        { hLocation = (var, chan)
        , hPush = \url -> do
            putStrLn $ "Router hPush: " <> T.unpack url

            atomically $ do
                writeTVar var (Location url)
                writeTChan chan (Location url)

            processSignals nauvaH
        }



runClient :: App -> IO ()
runClient app = do
    appEl <- getElementById ("app" :: JSString)

    nauvaH <- newHandle
    routerH <- newRouterH nauvaH

    bridge <- newBridge appEl $ Impl
        { sendLocationImpl = \path -> void $ hPush routerH path
        , componentEventImpl = \path val -> void $ dispatchComponentEventHandler nauvaH path val
        , nodeEventImpl = \path val -> void $ dispatchNodeEventHandler nauvaH path val
        , attachRefImpl = \path val -> void $ attachRefHandler nauvaH path val
        , detachRefImpl = \path val -> void $ detachRefHandler nauvaH path val
        , componentDidMountImpl = \path vals -> void $ componentDidMountHandler nauvaH path vals
        , componentWillUnmountImpl = \path vals -> void $ componentWillUnmountHandler nauvaH path vals
        }

    headH <- newHeadH nauvaH bridge
    appH <- AppH <$> pure headH <*> pure routerH
    render nauvaH (rootElement app appH)

    locationSignalCopy <- atomically $ dupTChan (snd $ hLocation routerH)
    void $ forkIO $ forever $ do
        path <- atomically $ do
            locPathname <$> readTChan locationSignalCopy

        pushLocation bridge (jsval $ textToJSString path)


    changeSignalCopy <- atomically $ dupTChan (changeSignal nauvaH)
    void $ forkIO $ forever $ do
        change <- atomically $ readTChan changeSignalCopy

        case change of
            (ChangeRoot inst) -> do
                spine <- atomically $ do
                    instanceToJSVal inst
                    -- rootInstance <- readTMVar (hInstance nauvaH)
                    -- instanceToJSVal rootInstance

                renderSpine bridge spine
            (ChangeComponent path inst) -> do
                spine <- atomically $ instanceToJSVal inst
                renderSpineAtPath bridge (unPath path) spine

                -- spine <- atomically $ do
                --     rootInstance <- readTMVar (hInstance nauvaH)
                --     instanceToJSVal rootInstance
                -- renderSpine bridge spine


    spine <- atomically $ do
        rootInstance <- readTMVar (hInstance nauvaH)
        instanceToJSVal rootInstance
    renderSpine bridge spine

    pure ()


foreign import javascript unsafe "console.log($1)" js_log :: JSVal -> IO ()


hookHandler :: Nauva.Handle.Handle -> Path -> JSVal -> IO (Either String ())
hookHandler h path vals = do
    res <- atomically $ runExceptT $ do
        SomeComponentInstance (ComponentInstance _ component stateRef) <- contextForPath h path
        state <- lift $ readTMVar stateRef

        let rawHookActions = fromMaybe [] (unsafePerformIO (fromJSVal vals)) :: [A.Value]
        forM rawHookActions $ \rawValue -> do
            case A.parseEither parseValue rawValue of
                Left e -> throwError e
                Right value -> do
                    actions <- lift $ do
                        state <- takeTMVar stateRef
                        let (newState, actions) = processLifecycleEvent component value (componentProps state) (componentState state)
                        (newInst, _effects) <- runWriterT $ instantiate path $ renderComponent component (componentProps state) newState
                        putTMVar stateRef (State (componentProps state) newState (componentSignals state) newInst)
                        -- traceShowM path
                        writeTChan (changeSignal h) (ChangeComponent path $ IComponent path component stateRef)
                        pure actions

                    pure $ Effect (ComponentInstance path component stateRef) actions

    case res of
        Left e -> pure $ Left e
        Right effects -> do
            executeEffects h effects
            pure $ Right ()


componentDidMountHandler :: Nauva.Handle.Handle -> Path -> JSVal -> IO (Either String ())
componentDidMountHandler = hookHandler

componentWillUnmountHandler :: Nauva.Handle.Handle -> Path -> JSVal -> IO (Either String ())
componentWillUnmountHandler = hookHandler

attachRefHandler :: Nauva.Handle.Handle -> Path -> JSVal -> IO ()
attachRefHandler h path jsVal = do
    res <- runExceptT $ do
        rawValue <- lift (fromJSVal jsVal) >>= maybe (throwError "fromJSVal") pure
        ExceptT $ dispatchRef h path rawValue

    case res of
        Left e -> putStrLn $ "attachRefHandler: " <> e
        Right () -> pure ()


detachRefHandler :: Nauva.Handle.Handle -> Path -> JSVal -> IO ()
detachRefHandler h path jsVal = do
    res <- runExceptT $ do
        rawValue <- lift (fromJSVal jsVal) >>= maybe (throwError "fromJSVal") pure
        ExceptT $ dispatchRef h path rawValue

    case res of
        Left e -> putStrLn $ "detachRefHandler: " <> e
        Right () -> pure ()


dispatchNodeEventHandler :: Nauva.Handle.Handle -> Path -> JSVal -> IO ()
dispatchNodeEventHandler h path jsVal = do
    res <- runExceptT $ do
        rawValue <- lift (fromJSVal jsVal) >>= maybe (throwError "fromJSVal") pure
        ExceptT $ dispatchEvent h path rawValue

    case res of
        Left e -> putStrLn $ "dispatchNodeEventHandler: " <> e
        Right () -> pure ()


dispatchComponentEventHandler :: Nauva.Handle.Handle -> Path -> JSVal -> IO ()
dispatchComponentEventHandler h path jsVal = do
    res <- runExceptT $ do
        rawValue <- lift (fromJSVal jsVal) >>= maybe (throwError "fromJSVal") pure
        ExceptT $ dispatchEvent h path rawValue

    case res of
        Left e -> putStrLn $ "dispatchComponentEventHandler: " <> e
        Right () -> pure ()






foreign import javascript unsafe "$r = $1"
    js_intJSVal :: Int -> JSVal
foreign import javascript unsafe "$r = $1"
    js_doubleJSVal :: Double -> JSVal

foreign import javascript unsafe "true"
    js_true :: JSVal
foreign import javascript unsafe "false"
    js_false :: JSVal
foreign import javascript unsafe "$r = null"
    js_null :: JSVal

jsCondition :: Condition -> JSVal
jsCondition (CMedia    x) = jsval $ fromList [js_intJSVal 1, jsval $ textToJSString x]
jsCondition (CSupports x) = jsval $ fromList [js_intJSVal 2, jsval $ textToJSString x]

jsCSSRule :: CSSRule -> JSVal
jsCSSRule (CSSStyleRule name hash conditions suffixes styleDeclaration) = jsval $ fromList
    [ js_intJSVal 1
    , jsval $ textToJSString name
    , jsval $ textToJSString $ unHash hash
    , jsval $ fromList $ map jsCondition conditions
    , jsval $ fromList $ map (jsval . JSS.pack . T.unpack . unSuffix) suffixes
    , unsafePerformIO $ do
        o <- O.create

        forM_ styleDeclaration $ \(k, v) -> do
            O.setProp (JSS.pack $ T.unpack k) (jsval $ JSS.pack $ T.unpack $ unCSSValue v) o

        pure $ jsval o
    ]
jsCSSRule (CSSFontFaceRule hash styleDeclaration) = jsval $ fromList
    [ js_intJSVal 5
    , jsval $ textToJSString $ unHash hash
    , unsafePerformIO $ do
        o <- O.create

        forM_ styleDeclaration $ \(k, v) -> do
            O.setProp (JSS.pack $ T.unpack k) (jsval $ JSS.pack $ T.unpack $ unCSSValue v) o

        pure $ jsval o
    ]

fToJSVal :: F -> JSVal
fToJSVal f = unsafePerformIO $ do
    o <- O.create
    O.setProp "id" (jsval $ textToJSString $ unFID $ fId f) o
    O.setProp "constructors" (jsval $ fromList $ map (jsval . textToJSString) $ fConstructors f) o
    O.setProp "arguments" (jsval $ fromList $ map (jsval . textToJSString) $ fArguments f) o
    O.setProp "body" (jsval $ textToJSString $ fBody f) o
    pure $ jsval o

instanceToJSVal :: Instance -> STM JSVal
instanceToJSVal = go []
  where
    go :: [Key] -> Instance -> STM JSVal
    go path inst = case inst of
        (INull _) -> pure js_null

        (IText _ text) -> pure $ jsval $ textToJSString text

        (INode _ tag attrs children) -> do
            newChildren <- forM children $ \(key, childI) -> do
                newChild <- instanceToJSVal childI
                key' <- case key of
                    (KIndex  i) -> pure $ js_intJSVal i
                    (KString s) -> pure $ (jsval $ textToJSString s)

                pure $ jsval $ fromList [key', newChild]


            pure $ unsafePerformIO $ do
                o <- O.create

                attributes' <- pure $ jsval $ fromList $ flip map attrs $ \x -> case x of
                    AVAL an (AVBool b)        -> jsval $ fromList [jsval $ textToJSString "AVAL", jsval $ textToJSString an, if b then js_true else js_false]
                    AVAL an (AVString s)      -> jsval $ fromList [jsval $ textToJSString "AVAL", jsval $ textToJSString an, jsval $ textToJSString s]
                    AVAL an (AVInt i)         -> jsval $ fromList [jsval $ textToJSString "AVAL", jsval $ textToJSString an, js_intJSVal i]
                    AVAL an (AVDouble d)      -> jsval $ fromList [jsval $ textToJSString "AVAL", jsval $ textToJSString an, js_doubleJSVal d]

                    AEVL (EventListener n f)  -> jsval $ fromList
                        [ jsval $ textToJSString "AEVL"
                        , jsval $ textToJSString n
                        , fToJSVal f
                        ]

                    ASTY style                -> jsval $ fromList
                        [ jsval $ textToJSString "ASTY"
                        , jsval $ fromList $ map jsCSSRule (unStyle style)
                        ]

                    AREF (Ref mbRefKey fra frd) -> unsafePerformIO $ do
                        o <- O.create

                        case mbRefKey of
                            Nothing -> pure ()
                            Just (RefKey k) -> O.setProp "key" (js_intJSVal $ k) o

                        O.setProp "attach" (fToJSVal fra) o
                        O.setProp "detach" (fToJSVal frd) o

                        pure $ jsval $ fromList [jsval $ textToJSString "AREF", jsval o]

                O.setProp "type" (jsval ("Node" :: JSString)) o
                O.setProp "tag" (jsval $ textToJSString $ unTag tag) o
                O.setProp "attributes" attributes' o
                O.setProp "children" (jsval $ fromList newChildren) o

                pure $ jsval o

        (IThunk _ _ _ childI) ->
            instanceToJSVal childI

        (IComponent _ component stateRef) -> do
            state <- readTMVar stateRef
            spine <- instanceToJSVal $ componentInstance state

            eventListeners' <- pure $ jsval $ fromList $ flip map (componentEventListeners component (componentState state)) $ \el -> case el of
                (EventListener n f) -> jsval $ fromList
                    [ jsval $ textToJSString n
                    , fToJSVal f
                    ]

            pure $ unsafePerformIO $ do
                o <- O.create

                hooks <- O.create

                O.setProp "componentDidMount" (jsval $ fromList $ map fToJSVal (componentDidMount (componentHooks component))) hooks
                O.setProp "componentWillUnmount" (jsval $ fromList $ map fToJSVal (componentWillUnmount (componentHooks component))) hooks

                O.setProp "type" (jsval ("Component" :: JSString)) o
                O.setProp "id" (js_intJSVal $ unComponentId $ componentId component) o
                O.setProp "displayName" (jsval $ textToJSString $ componentDisplayName component) o
                O.setProp "eventListeners" eventListeners' o
                O.setProp "hooks" (jsval hooks) o
                O.setProp "spine" spine o

                pure $ jsval o
