{-# LANGUAGE OverloadedStrings #-}

module Nauva.Handle
    ( Handle(..)
    , newHandle

    , Change(..)

    , instantiate
    , executeEffects
    , applyAction
    , contextForPath
    , processSignals

    , render
    , dispatchEvent
    , dispatchHook
    , dispatchRef
    , toSpine

    , createSnapshot
    , restoreSnapshot
    ) where


import           Data.List
import           Data.Map         (Map)
import qualified Data.Map         as M
import qualified Data.Aeson       as A
import qualified Data.Aeson.Types as A
import           Data.Traversable
import           Data.Foldable
import           Data.Function
import           Data.Typeable

import           Control.Monad.Except
import           Control.Monad.Writer
import           Control.Concurrent
import           Control.Concurrent.STM

import           Prelude

import           Nauva.Internal.Types
import           Nauva.NJS


--------------------------------------------------------------------------------
-- | A 'Handle' in Nauva is like a DOM element in React: You can render an
-- 'Element' into it, it keeps the current (instantiated) state of the
-- 'Element', you can send events into it (which you receive from a browser,
-- for example). You can get notified when something the instantiated tree
-- changes.

data Handle = Handle
    { changeSignal :: TChan Change

    , hInstance :: TMVar Instance
      -- ^ The root instance is hidden inside a 'TMVar'. This is used as
      -- a mutex to ensure that only one thread is applying changes to it.
      --
      -- Note though that the state of components within this instance can be
      -- updated at any time by other thread. They don't need to acquire this
      -- mutex.
    }


data Change
    = ChangeRoot Instance
    | ChangeComponent Path Instance


-- | Create a new 'Handle' with an empty 'Instance' in it.
newHandle :: IO Handle
newHandle = Handle
    <$> newBroadcastTChanIO
    <*> newTMVarIO (IText (Path []) "")



-- | Render an 'Element' into the 'Handle'. The function will intelligently
-- sync the current 'Instance' inside the 'Handle' to match she shape of the
-- given 'Element'.
--
-- If you maintain your application state elsewhere, you will repeatedly call
-- this function with a new 'Element'. But you can also use stateful
-- 'Component's and only send external events (DOM and React lifecycle events,
-- see 'dispatchEvent') into the 'Handle', and let the 'Component's manage
-- the state for you.

render :: Handle -> Element -> IO ()
render h rootElement = do
    effects <- atomically $ do
        currentInstance <- takeTMVar (hInstance h)
        (newInstance, effects) <- runWriterT $ go [] rootElement currentInstance
        putTMVar (hInstance h) newInstance
        writeTChan (changeSignal h) (ChangeRoot newInstance)
        pure effects

    executeEffects h effects


  where
    instantiate' :: Path -> Element -> WriterT [Effect] STM Instance
    instantiate' path el = do
        (inst, effects) <- lift $ instantiate path el
        tell effects
        pure inst

    go :: [Key] -> Element -> Instance -> WriterT [Effect] STM Instance
    go path el@(EText eText) inst@(IText _ iText) = if eText == iText
        then pure inst
        else instantiate' (Path path) el

    go path (ENode eTag eAttrs eChildren) (INode _ _ _ iChildren) = do
        (newChildren, _remainingChildren) <- foldlM (\(newChildren, oldChildren) (i, childE) -> do
            let key = KIndex i
            case M.lookup key oldChildren of
                Nothing -> do
                    childI <- instantiate' (Path (path ++ [key])) childE
                    -- lift $ writeTChan (changeSignal h) ()
                    pure (newChildren ++ [(key, childI)], oldChildren)
                Just oldChildI -> do
                    childI <- go (path <> [key]) childE oldChildI
                    pure (newChildren ++ [(key, childI)], M.delete key oldChildren)
            ) ([], M.fromList iChildren) (zip [1..] eChildren)

        -- TODO: dispose remainingChildren

        pure $ INode (Path path) eTag eAttrs newChildren

    go path el@(EThunk eComp eProps) inst@(IThunk _ iComp iProps _) = do
        if not (shouldThunkUpdate' eComp eProps iComp iProps)
            then pure inst
            else do
                childI <- instantiate' (Path path) el
                -- lift $ writeTChan (changeSignal h) ()
                pure $ IThunk (Path path) eComp eProps childI

    go path el@(EComponent eApp eProps) inst@(IComponent _ iApp iState) = do
        case guard (componentId eApp == componentId iApp) >> cast eProps of
            Just newProps -> do
                effects <- lift $ sendProps (Path path) iApp iState newProps
                tell effects
                pure inst
            Nothing -> do
                -- lift $ writeTChan (changeSignal h) ()
                instantiate' (Path path) el

    go path el _ = do
        -- TODO: dispose inst
        -- writeTChan (changeSignal h) ()
        instantiate' (Path path) el



contextForPath :: Handle -> Path -> ExceptT String STM (Maybe SomeComponentInstance, Instance)
contextForPath h path = do
    currentInstance <- lift $ takeTMVar (hInstance h)
    res <- go Nothing path currentInstance
    lift $ putTMVar (hInstance h) currentInstance
    pure res

  where
    go :: Maybe SomeComponentInstance -> Path -> Instance -> ExceptT String STM (Maybe SomeComponentInstance, Instance)
    go mbSCI (Path []) inst = case inst of
        (INull _)                         -> pure (mbSCI, inst)
        (IText _ _)                       -> pure (mbSCI, inst)
        (INode _ _ _ _)                   -> pure (mbSCI, inst)
        (IThunk _ _ _ childI)             -> go mbSCI (Path []) childI
        (IComponent _ component stateRef) -> do
            state <- lift $ readTMVar stateRef
            let sci = SomeComponentInstance $ ComponentInstance path component stateRef
            go (Just sci) (Path []) $ componentInstance state

    go mbSCI (Path (key:rest)) inst = case inst of
        (INull _) -> do
            throwError $ "contextForPath: INull doesn't have any children"

        (IText _ _) -> do
            throwError $ "contextForPath: IText doesn't have any children"

        (INode _ _ _ children) -> do
            case lookup key children of
                Nothing -> throwError $ "contextForPath: Child at key " ++ show key ++ " not found"
                Just childI -> go mbSCI (Path rest) childI

        (IThunk _ _ _ childI) ->
            go mbSCI (Path (key:rest)) childI

        (IComponent _ component stateRef) -> do
            state <- lift $ readTMVar stateRef
            let sci = SomeComponentInstance $ ComponentInstance (Path $ take (length (unPath path) - length rest - 1) $ unPath path) component stateRef
            go (Just sci) (Path (key:rest)) $ componentInstance state




-- | Send an event to the 'Instance' which is located at the given @path@
-- (represented as a list of 'Key's). The event is given encoded as a JSON
-- 'Value' (because we need a generic representation of it which is compatible
-- with all 'Component's).
--
-- Events are handled by the closest 'Component' ancestor. If there is none,
-- then the event is ignored and the function returns 'Left'.
dispatchEvent :: Handle -> Path -> A.Value -> IO (Either String ())
dispatchEvent h path rawEvent = do
    res <- atomically $ runExceptT $ do
        (mbSCI, inst) <- contextForPath h path
        case (mbSCI, inst) of
            (Just (SomeComponentInstance ci), _) -> do
                lift $ case A.parseEither parseValue (taggedWithAction (ciComponent ci) rawEvent) of
                    Left e -> error $ show e
                    Right action -> applyAction h action ci
            _ -> throwError $ "dispatchEvent: no context for path " ++ show path

    case res of
        Left e -> pure $ Left e
        Right effects -> do
            executeEffects h effects
            pure $ Right ()


dispatchHook :: Handle -> Path -> A.Value -> IO (Either String ())
dispatchHook h path rawValue = do
    res <- atomically $ do
        currentInstance <- takeTMVar (hInstance h)
        res <- runExceptT $ go path currentInstance
        putTMVar (hInstance h) currentInstance
        pure res

    case res of
        Left e -> pure $ Left e
        Right effects -> do
            executeEffects h effects
            pure $ Right ()

  where
    go :: Path -> Instance -> ExceptT String STM [Effect]
    go (Path []) inst = case inst of
        (INull _) ->
            throwError $ "Can not dispatch hook to INull (at path " ++ show path ++ ")"

        (IText _ _) ->
            throwError $ "Can not dispatch hook to IText (at path " ++ show path ++ ")"

        (INode _ _ _ _) ->
            throwError $ "Can not dispatch hook to INode (at path " ++ show path ++ ")"

        (IThunk _ _ _ childI) ->
            go (Path []) childI

        (IComponent p component stateRef) -> do
            case A.parseEither parseValue (taggedWithHook component rawValue) of
                Left e -> throwError e
                Right value -> lift $ do
                    state <- takeTMVar stateRef
                    let (newState, actions) = processLifecycleEvent component value (componentProps state) (componentState state)
                    (newInst, effects) <- instantiate p $ renderComponent component (componentProps state) newState
                    putTMVar stateRef (State (componentProps state) newState (componentSignals state) newInst)
                    writeTChan (changeSignal h) (ChangeComponent path inst)
                    pure $ effects <> [Effect (ComponentInstance path component stateRef) actions]

    go (Path (key:rest)) inst = case inst of
        (INull _) -> do
            throwError $ "INull doesn't have any children"

        (IText _ _) -> do
            throwError $ "IText doesn't have any children"

        (INode _ _ _ children) -> do
            case lookup key children of
                Nothing -> throwError $ "Child at key " ++ show key ++ " not found"
                Just childI -> go (Path rest) childI

        (IThunk _ _ _ childI) ->
            go (Path rest) childI

        (IComponent _ _ stateRef) -> do
            state <- lift $ readTMVar stateRef
            go (Path rest) (componentInstance state)


dispatchRef :: Handle -> Path -> A.Value -> IO (Either String ())
dispatchRef h path rawValue = do
    res <- atomically $ do
        currentInstance <- takeTMVar (hInstance h)
        res <- runExceptT $ go ([], currentInstance) path currentInstance
        putTMVar (hInstance h) currentInstance
        pure res

    case res of
        Left e -> pure $ Left e
        Right effects -> do
            executeEffects h effects
            pure $ Right ()

  where
    go :: ([Key], Instance) -> Path -> Instance -> ExceptT String STM [Effect]
    go (appPath, appAncestor) (Path []) inst = case inst of
        (INull _) ->
            throwError $ "Can not dispatch ref to a INull node (at path " ++ show path ++ ")"

        (IText _ _) ->
            throwError $ "Can not dispatch ref to a IText node (at path " ++ show path ++ ")"

        (INode _ _ _ _) -> case appAncestor of
            -- We've reached the native element which emitted the event.
            -- Dispatch it to the closest 'IComponent' ancestor (if there is one).
            (INull _) -> throwError $ "No App is ancestor of " ++ show path
            (IText _ _) -> throwError $ "No App is ancestor of " ++ show path
            (INode _ _ _ _) -> throwError $ "No App is ancestor of " ++ show path
            (IThunk _ _ _ _) -> throwError $ "No App is ancestor of " ++ show path
            (IComponent _ component stateRef) -> lift $ case A.parseEither parseValue (taggedWithAction component rawValue) of
                Left e -> error $ show e
                Right action -> applyAction h action (ComponentInstance (Path appPath) component stateRef)

        (IThunk _ _ _ childI) ->
            go (appPath, appAncestor) (Path []) childI

        (IComponent _ _ stateRef) -> do
            state <- lift $ readTMVar stateRef
            go (appPath, inst) (Path []) (componentInstance state)

    go (appPath, appAncestor) (Path (key:rest)) inst = case inst of
        (INull _) -> do
            throwError $ "INull doesn't have any children"

        (IText _ _) -> do
            throwError $ "IText doesn't have any children"

        (INode _ _ _ children) -> do
            case lookup key children of
                Nothing -> throwError $ "Child at key " ++ show key ++ " not found"
                Just childI -> go (appPath, appAncestor) (Path rest) childI

        (IThunk _ _ _ childI) ->
            go (appPath, appAncestor) (Path rest) childI

        (IComponent _ _ stateRef) -> do
            state <- lift $ readTMVar stateRef
            go (take (length (unPath path) - length rest - 1) (unPath path), inst) (Path rest) $ componentInstance state



-- | Convert an 'Instance' into a 'Spine'. This function runs in 'STM' because
-- 'IComponent's store their instance in a 'TMVar' and this function needs to
-- extract the instance from it.
toSpine :: Instance -> STM Spine
toSpine inst = case inst of
    (INull _) -> pure SNull
    (IText _ text) -> pure $ SText text

    (INode _ tag attrs children) -> do
        newChildren <- forM children $ \(key, childI) -> do
            newChild <- toSpine childI
            pure (key, newChild)

        pure $ SNode tag attrs newChildren

    (IThunk _ _ _ childI) ->
        toSpine childI

    (IComponent _ component stateRef) -> do
        state <- readTMVar stateRef
        spine <- toSpine $ componentInstance state

        pure $ SComponent
            (componentId component)
            (componentDisplayName component)
            (componentEventListeners component $ componentState state)
            (componentHooks component)
            spine




sendProps :: (Typeable p, Value h, Value a) => Path -> Component p h s a -> TMVar (State p s a) -> p -> STM [Effect]
sendProps path component stateRef newProps = do
    state <- takeTMVar stateRef
    (newState, signals, actions) <- receiveProps component newProps (componentState state)
    (inst, effects) <- instantiate path $ renderComponent component (componentProps state) newState
    putTMVar stateRef $ State newProps newState signals inst
    pure $ [Effect (ComponentInstance path component stateRef) actions] <> effects



applyAction :: (Typeable p, Value h, Value a) => Handle -> a -> ComponentInstance p h s a -> STM [Effect]
applyAction h action (ComponentInstance path component stateRef) = do
    state <- takeTMVar stateRef
    let (newState, actions) = update component action (componentProps state) (componentState state)
    (newInst, effects) <- instantiate path $ renderComponent component (componentProps state) newState
    putTMVar stateRef (State (componentProps state) newState (componentSignals state) newInst)
    writeTChan (changeSignal h) (ChangeComponent path $ IComponent path component stateRef)
    pure $ effects <> [Effect (ComponentInstance path component stateRef) actions]



-- | Create an 'Instance' which corresponds to the given 'Element'. This runs
-- in 'STM', because we need to allocate new 'TMVar's to store 'Component'
-- state ('State') for newly allocated 'IComponent' instances.
instantiate :: Path -> Element -> STM (Instance, [Effect])
instantiate path el = case el of
    (ENull) -> pure (INull path, [])

    (EText t) -> pure (IText path t, [])

    (ENode tag attributes children) -> do
        childInstances <- forM (zip (map KIndex [1..]) children) $ \(key, child) -> do
            (childInstance, effects) <- instantiate (Path (unPath path <> [key])) child
            pure ((key, childInstance), effects)

        pure (INode path tag attributes (map fst childInstances), mconcat $ map snd childInstances)


    (EThunk thunk p) -> do
        (inst, effects) <- instantiate path (forceThunk thunk p)
        pure (IThunk path thunk p inst, effects)

    (EComponent component p) -> do
        (s, signals, actions) <- initialComponentState component p
        (inst, effects) <- instantiate path $ renderComponent component p s
        stateVar <- newTMVar (State p s signals inst)
        pure (IComponent path component stateVar, [Effect (ComponentInstance path component stateVar) actions] <> effects)


executeEffects :: Handle -> [Effect] -> IO ()
executeEffects h effects = do
    forM_ effects $ \(Effect ci actions) -> do
        forM_ actions $ \m -> forkIO $ do
            mbA <- m
            case mbA of
                Nothing -> pure ()
                Just a -> do
                    nextEffects <- atomically $ applyAction h a ci
                    executeEffects h nextEffects

    atomically $ do
        rootInstance <- readTMVar (hInstance h)
        writeTChan (changeSignal h) (ChangeRoot rootInstance)



--------------------------------------------------------------------------------
-- | A Snapshot captures the states of all 'Component's. You can serialize it
-- write to disk and restore later.
--
-- Components can decide what to store in the snapshot. Not everything makes
-- sense, which we don't require the 'Component' state to be serializable.
-- Instead, each 'Component' provides two functions for serializing and loading
-- data to/from the snapshot.
--
-- Internally, a 'Snapshot' is a map from component instance paths to their
-- serialized state snapshot.

newtype Snapshot = Snapshot { unSnapshot :: Map [Key] A.Value }

instance A.ToJSON Snapshot where
    toJSON = A.toJSON . M.toList . unSnapshot

instance A.FromJSON Snapshot where
    parseJSON v = Snapshot . M.fromList <$> A.parseJSON v


-- | Create a new snapshot of the application at this point in time.
createSnapshot :: Handle -> STM Snapshot
createSnapshot h = Snapshot <$> execWriterT (go <$> (lift (readTMVar (hInstance h))))
  where
    go :: Instance -> WriterT (Map [Key] A.Value) STM ()
    go inst = case inst of
        (INull _) ->
            pure ()

        (IText _ _) ->
            pure ()

        (INode _ _ _ children) ->
            mapM_ (go <$> snd) children

        (IThunk _ _ _ childI) ->
            go childI

        (IComponent path component stateRef) -> do
            State _ s _ _ <- lift $ readTMVar stateRef
            tell $ M.singleton (unPath path) $ componentSnapshot component s


-- | Restore the state of the application from the snapshot. If a component
-- fails to restore its state from the snapshot (for example because the format
-- has changed), then that component is ignored and its state will remain
-- untouched.
restoreSnapshot :: Handle -> Snapshot -> IO ()
restoreSnapshot h snapshot = do
    effects <- atomically $ do
        rootInstance <- takeTMVar (hInstance h)
        effects <- execWriterT $ go [] rootInstance
        putTMVar (hInstance h) rootInstance
        pure effects

    executeEffects h effects

  where
    go :: [Key] -> Instance -> WriterT [Effect] STM ()
    go path inst = case inst of
        (INull _) -> pure ()

        (IText _ _) -> pure ()

        (INode _ _ _ children) -> do
            forM_ children $ \(key, child) ->
                go (path <> [key]) child

        (IThunk _ _ _ childI) ->
            go path childI

        (IComponent _ component stateRef) -> do
            state <- lift $ takeTMVar stateRef
            newState <- case M.lookup path (unSnapshot snapshot) of
                Nothing -> pure state
                Just value -> do
                    case restoreComponent component value (componentState state) of
                        Left _ -> pure state
                        Right (newState, actions) -> do
                            tell [Effect (ComponentInstance (Path path) component stateRef) actions]
                            (newInst, effects) <- lift $ instantiate (Path path) $ renderComponent component (componentProps state) newState
                            tell effects
                            pure $ State (componentProps state) newState (componentSignals state) newInst

            lift $ putTMVar stateRef newState
            go path (componentInstance newState)



-- | Go through the instance tree once, and process all signal channels which
-- have a value in them that is ready to be consumed.
--
-- The tree is traversed breadth-first.

processSignals :: Handle -> IO ()
processSignals h = do
    effects <- atomically $ do
        currentInstance <- takeTMVar (hInstance h)
        someSignals <- execWriterT $ go currentInstance
        putTMVar (hInstance h) currentInstance

        effects <- forM someSignals $ \(SomeSignal ci@(ComponentInstance path component stateRef) (Signal chan f)) -> do
            mbA <- tryReadTChan chan
            case mbA of
                Nothing -> pure []
                Just a  -> do
                    state <- takeTMVar stateRef
                    let (newState, actions) = f a (componentProps state) (componentState state)
                    (newInst, effects) <- instantiate path $ renderComponent component (componentProps state) newState
                    putTMVar stateRef (State (componentProps state) newState (componentSignals state) newInst)
                    writeTChan (changeSignal h) (ChangeComponent path $ IComponent path component stateRef)
                    pure $ [Effect ci actions] <> effects

        pure $ mconcat effects

    executeEffects h effects

  where
    go :: Instance -> WriterT [SomeSignal] STM ()
    go inst = case inst of
        (INull _) -> pure ()

        (IText _ _) -> pure ()

        (INode _ _ _ children) ->
            mapM_ (go <$> snd) children

        (IThunk _ _ _ childI) ->
            go childI

        (IComponent path component stateRef) -> do
            (State _ _ signals childI) <- lift $ readTMVar stateRef
            tell $ map (SomeSignal (ComponentInstance path component stateRef)) signals
            go childI
