{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Nauva.Internal.Types where


import           Data.List
import           Data.IORef
import           Data.Aeson (ToJSON(..), FromJSON(..), (.=), object)
import qualified Data.Aeson as A
import           Data.Function
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable
import           Data.Tagged
import           Data.String

import           Control.Monad.Writer.Lazy
import           Control.Concurrent.STM

import           System.IO.Unsafe

import           Prelude

import           Nauva.DOM
import           Nauva.Internal.Events
import           Nauva.NJS (Value, FID, F1(..), F2(..), FRA, FRD, createF, njsCon0, holeE, value0E)
import           Nauva.CSS.Types


--------------------------------------------------------------------------------
-- | The Nauva virtual DOM is made out of these elements. The 'EText' and
-- 'ENode' elements map directly to DOM elements. The two other types are used
-- by Nauva to implement efficient updates of sub-trees.

data Element where
    ENull :: Element
    -- An element which is not shown. Acts as a placeholder for when you want
    -- to toggle an element (show/hide depending on a condition).

    EText :: Text -> Element
    -- A text element.

    ENode :: Tag -> [Attribute] -> [Element] -> Element
    -- An element representing a native DOM node.

    EThunk :: (Typeable p) => Thunk p -> p -> Element
    -- An element which is backed by a pure function @p -> Element@. The
    -- function is only evaluated when needed.
    --
    -- Called @Stateless Functional Component@ (SFC) in React and @Thunk@ in
    -- the virtual-dom documentation.

    EComponent :: (Typeable p, FromJSON a, Value h, Value a) => Component p h s a -> p -> Element
    -- A stateful component that can be embededd into a larger Element. The
    -- component maintains its own, local state which is opaque to its parents.



--------------------------------------------------------------------------------
-- | The allocated instances from the 'Element' tree. This tree contain the
-- 'State' objects of 'Component's. You can think of this as the complete
-- representation of your application's state, which includes UI-specific bits
-- and picese of state.

data Instance where
    INull :: Instance

    IText :: Text -> Instance

    INode :: Tag -> [Attribute] -> [(Key, Instance)] -> Instance

    IThunk :: (Typeable p) => Thunk p -> p -> Instance -> Instance
    -- In the instance for 'EThunk', we remember the forced and instantiated
    -- result of the thunk.

    IComponent :: (Typeable p, FromJSON a, Value h, Value a) => Component p h s a -> TMVar (State p s a) -> Instance


-------------------------------------------------------------------------------
-- | A 'Spine' is a simplified structure of the 'Instance' tree. It contains
-- just enough information so that the JavaScript glue code in the web browser
-- can reconstruct the React VDOM tree, render it into the actual DOM, knows
-- which events to capture and how to send them back to the server.
--
-- The 'Spine' doesn't contain 'Thunk's as separate objects, 'Thunk's are
-- forced and converted to their underlying 'Instance' (and then 'Spine').
--
-- 'Spine' is a sufficiently abstract data type that can be encoded into JSON.

data Spine where
    SNull :: Spine
    SText :: Text -> Spine
    SNode :: Tag ->  [Attribute] -> [(Key, Spine)] -> Spine
    SComponent :: ComponentId -> Text -> [EventListener] -> Hooks h -> Spine -> Spine


instance A.ToJSON Spine where
    toJSON s = case s of
        (SNull) -> A.Null

        (SText text) -> toJSON text

        (SNode tag attrs children) -> object
            [ "type" .= ("Node" :: String)
            , "tag" .= toJSON tag
            , "attributes" .= toJSON (map toJSON attrs)
            , "children" .= toJSON (map toJSON children)
            ]

        (SComponent (ComponentId cId) displayName eventListeners hooks spine) -> object
            [ "type" .= ("Component" :: String)
            , "id" .= toJSON cId
            , "displayName" .= toJSON displayName
            , "eventListeners" .= toJSON (map toJSON eventListeners)
            , "hooks" .= toJSON hooks
            , "spine" .= toJSON spine
            ]



--------------------------------------------------------------------------------
-- | A 'Key' is used to identify children of an 'ENode' 'Element'. Keys should
-- be unique within the scope of their parent. They are used to optimize updates
-- of child elements when they are merely shuffled around, or when a new child
-- is inserted or removed from the middle of the list.
--
-- Keys can be either integers or strings. The default is to use the index
-- if the child in the list, but users can override it if they can provide a
-- more stable key, such as deriving it from an internal identifier in their
-- domain model.

data Key = KIndex Int | KString Text
    deriving (Eq, Ord)

instance Show Key where
    show (KIndex  i) = show i
    show (KString s) = T.unpack s

instance ToJSON Key where
    toJSON (KIndex  i) = toJSON i
    toJSON (KString s) = toJSON s

instance FromJSON Key where
    parseJSON (A.String s) = pure $ KString s
    parseJSON v            = KIndex <$> parseJSON v


--------------------------------------------------------------------------------
-- | A 'Path' is a list of 'Key's and is used to uniquely identify an instance
-- in the Nauva Virtual DOM.

newtype Path = Path { unPath :: [Key] }

instance Show Path where
    show = show . unPath

instance ToJSON Path where
    toJSON = toJSON . unPath

instance FromJSON Path where
    parseJSON v = Path <$> parseJSON v



--------------------------------------------------------------------------------
-- | Refs are used to capture the actual DOM nodes in your application so you
-- can access them directly. This is useful if you want to use DOM APIs which
-- are not exposed in Nauva / React. Or if you want to integrate a third-party
-- library into the project and that library needs access to the real DOM nodes.
--
-- A 'Ref' contains two function expressions: one is evaluated after the DOM
-- node was inserted into the document, the other after the DOM node was
-- removed. You don't have access to the DOM node in the later, but you should
-- use that opportunity to clean up any resources.
--
-- In addition to the two function expressions, the 'Ref' also specifies under
-- which key the node should be stored in the component. Other expressions
-- (such as event handlers) can use that key to refer to the native DOM node.

data Ref where
    Ref :: (Typeable r, Value r) => Maybe RefKey -> FRA a b r -> FRD a r -> Ref

instance ToJSON Ref where
    toJSON (Ref key attach detach) = object
        [ "key"    .= key
        , "attach" .= f2Fn attach (holeE 0) (holeE 1)
        , "detach" .= f1Fn detach (holeE 0)
        ]


--------------------------------------------------------------------------------
-- | A unique key identifying a 'Ref'. Use 'mkRefKey' to create new 'RefKey's.

newtype RefKey = RefKey { unRefKey :: Int }
    deriving (Eq, Ord)

instance ToJSON RefKey where
    toJSON = A.toJSON . unRefKey


refKeyCounter :: IORef Int
refKeyCounter = unsafePerformIO $ newIORef 1
{-# NOINLINE refKeyCounter #-}

mkRefKey :: RefKey
mkRefKey = RefKey $ unsafePerformIO $
    atomicModifyIORef' refKeyCounter $ \i -> (i + 1, i)



--------------------------------------------------------------------------------
-- | Thunks are in essence pure functions which render an 'Element'. Each
-- 'Thunk' is given a unique 'ThunkId' (because we only export a smart
-- constructor for 'ThunkId', which is guaranteed to generate a unique ID each
-- time it is used). Only thunks with the same 'ThunkId' are considered
-- compatible. If two thunks have the same 'forceThunk' function but a
-- different 'thunkId', Nauva will assume that they are incompatible.
--
-- Due to these restrictions, it only makes sense when you can statically
-- create these 'Thunk' objects. If you create them on-the-fly, Nauva loses the
-- ability to optimize rendering. Use @NOINLINE@ pragmas to prevent GHC from
-- inlining the code.

data Thunk p = Thunk
    { thunkId :: ThunkId
      -- ^ A unique ID of this thunk.
    , thunkDisplayName :: String
      -- ^ Useful for debugging. This string is shown in the React DevTools.
    , shouldThunkUpdate :: p -> p -> Bool
      -- ^ If this function returns 'False', then rendering of the 'Thunk'
      -- (and its whole subtree) is skipped. If @p@ implements the 'Eq'
      -- typeclass, you can use '=='.
    , forceThunk :: p -> Element
      -- ^ Force evaluation of this thunk.
    }


createThunk :: (ThunkId -> Thunk p) -> Thunk p
createThunk f = unsafePerformIO $ do
    tId <- atomicModifyIORef' thunkIdCounter $ \i -> (i + 1, i)
    pure $ f $ ThunkId tId


-- | Create a simple 'Thunk' out of a function which takes an argument of some
-- type @p@ and turns that into an 'Element'. The @p@ type must implement the
-- 'Eq' typeclass, because '==' is used as the implementation of
-- 'shouldThunkUpdate'.
simpleThunk :: Eq p => String -> (p -> Element) -> Thunk p
simpleThunk dispName f = createThunk $ \tId -> Thunk tId dispName (==) f


-- | This function creates a 'Element' which is backed by a 'Thunk' internally.
-- If you have a larger tree which never changes (such as a SVG image, a
-- navigation element, the footer etc), you are encouraged to use this function
-- to create a CAF and use that throughout your application.
--
-- > footer :: Element
-- > footer = constElement "Footer" $ ENode "div" []
-- >     [ EText "Copyright 2016 ..."
-- >     , ...
-- >     ]

constElement :: String -> Element -> Element
constElement dispName el = EThunk (simpleThunk dispName (const el)) ()



--------------------------------------------------------------------------------
newtype ThunkId = ThunkId { unThunkId :: Int }
    deriving (Eq)

thunkIdCounter :: IORef Int
thunkIdCounter = unsafePerformIO $ newIORef 1
{-# NOINLINE thunkIdCounter #-}


shouldThunkUpdate' :: (Typeable a, Typeable b) => Thunk a -> a -> Thunk b -> b -> Bool
shouldThunkUpdate' ta a tb b = case cast a of
    Nothing -> True
    Just b' -> thunkId ta /= thunkId tb || shouldThunkUpdate tb b b'



--------------------------------------------------------------------------------
-- | Hooks into the React component lifecycle. Hooks generate values of a type
-- @h@, which are fed to the 'Component' (through the 'processLifecycleEvent'
-- function). Each 'Component' can decide on its own how to map these values
-- into state changes.
--
-- Each hook generates a list of NJS expression which are execued in sequence.

data Hooks h = Hooks
    { componentDidMount :: [ F1 () h ]
    , componentWillUnmount :: [ F1 () h ]
    }

instance ToJSON (Hooks h) where
    toJSON hooks = object
        [ "componentDidMount"
            .= fmap (\f -> toJSON (f1Fn f $ holeE 0))
                (componentDidMount hooks)
        , "componentWillUnmount"
            .= fmap (\f -> toJSON (f1Fn f $ holeE 0))
                (componentWillUnmount hooks)
        ]


-- | 'Hooks' which don't run anything.
emptyHooks :: Hooks ()
emptyHooks = Hooks
    { componentDidMount    = []
    , componentWillUnmount = []
    }

-- | 'Hooks' which generate a constant ('()' aka unit) each time one of the
-- lifecycle hooks is invoked.
constHooks :: Hooks ()
constHooks = Hooks
    { componentDidMount    = [ createF $ \fId -> F1 fId (\_ -> value0E unitC) ]
    , componentWillUnmount = [ createF $ \fId -> F1 fId (\_ -> value0E unitC) ]
    }
  where
    unitC = njsCon0 "()" ()


--------------------------------------------------------------------------------
-- | The definition of a 'Component'.
--
-- - @p@ is the type of props which are given to the component by its parent.
-- - @h@ is the type that the 'Hooks' produce.
-- - @s@ is the type of the internal state.
-- - @a@ is the type of the actions which this component produces.
-- - @ia@ is the type of actions which the component can consume. These actions
--     are generated by child elements of this component.

data Component p h s a = Component
    { componentId :: ComponentId
     -- ^ A unique ID which identifies this component.

    , componentDisplayName :: Text
      -- ^ Same purpose as 'thunkDisplayName'.

    , initialComponentState :: p -> STM (s, [Signal s a])
      -- ^ The initial state of the application may only depend on the props.

    , componentEventListeners :: s -> [EventListener]
      -- ^ These event listeners are attached to the global object (window).
      -- Most useful for tracking resize and scroll events.

    , componentHooks :: Hooks h
      -- ^ This describes how lifecycle events (on the client) are translated
      -- into values of type 'h'.

    , processLifecycleEvent :: h -> p -> s -> (s, [IO (Maybe a)])
      -- ^ Allows the App to act on lifecycle events.

    , receiveProps :: p -> s -> STM (s, [Signal s a], [IO (Maybe a)])
      -- ^ When the component receives new props from its parent. This only
      -- happens if the component is embedded inside inside another 'Element'
      -- in the tree.

    , update :: a -> p -> s -> (s, [IO (Maybe a)])

    , renderComponent :: p -> s -> Element

    , componentSnapshot :: s -> A.Value
      -- ^ Extract the essential parts from the state and generate a 'Value'
      -- for the 'Snapshot'.

    , restoreComponent :: A.Value -> s -> Either String (s, [IO (Maybe a)])
      -- ^ Restore the state from a 'Value' which is loaded from a 'Snapshot'.
    }


createComponent :: (ComponentId -> Component p h s a) -> Component p h s a
createComponent f = unsafePerformIO $ do
    cId <- atomicModifyIORef' componentIdCounter $ \i -> (i + 1, i)
    pure $ f $ ComponentId cId


lookupComponentEventListener :: FID -> Component p h s a -> s -> Maybe EventListener
lookupComponentEventListener fid component state = find
    (\(EventListener _ f) -> f1Id f == fid)
    (componentEventListeners component state)



--------------------------------------------------------------------------------
newtype ComponentId = ComponentId { unComponentId :: Int }
    deriving (Eq, Ord)

componentIdCounter :: IORef Int
componentIdCounter = unsafePerformIO $ newIORef 1
{-# NOINLINE componentIdCounter #-}



taggedWithAction :: Component p h s a -> A.Value -> Tagged a A.Value
taggedWithAction _ = Tagged

taggedWithHook :: Component p h s a -> A.Value -> Tagged h A.Value
taggedWithHook _ = Tagged



--------------------------------------------------------------------------------
-- | The state object which is allocated for a 'Component' when it is
-- instantiated. This object is stored in a 'IORef', so it can be mutated as
-- needed (when the component receives input from outside).

data State p s a = State
    { componentProps :: !p
    , componentState :: !s
    , componentSignals :: ![Signal s a]
    , componentInstance :: !Instance
    }



--------------------------------------------------------------------------------
-- | A 'Signal' is a pair of 'TChan' and a corresponding state update function.
-- Signals act as an additional source of inputs into a 'Component'.

data Signal s a where
    Signal :: TChan i -> (i -> s -> (s, [IO (Maybe a)])) -> Signal s a

data SomeSignal where
    SomeSignal :: (Typeable p, A.FromJSON a, Value h, Value a) => ComponentInstance p h s a -> Signal s a -> SomeSignal



--------------------------------------------------------------------------------
-- | An external effect which a 'Component' wishes to execute (usually as a
-- response to a state change). The effects are represented as 'IO' actions
-- which produce values of type @ia@, which are then fed back into the
-- component to update its state.
--
-- Each 'IO' action is executed in its own thread.

data Effect where
    Effect :: (Typeable p, FromJSON a, Value h, Value a) =>
        ComponentInstance p h s a -> [IO (Maybe a)] -> Effect



--------------------------------------------------------------------------------
-- | A component instance + the path to it. Quite often we need both, so that
-- changes to this component can generate 'ChangeComponent' changes.
data ComponentInstance p h s a = ComponentInstance
    { ciPath :: Path
    , ciComponent :: Component p h s a
    , ciState :: TMVar (State p s a)
    }

data SomeComponentInstance where
    SomeComponentInstance :: (Typeable p, FromJSON a, Value h, Value a) =>
        ComponentInstance p h s a -> SomeComponentInstance





--------------------------------------------------------------------------------
-- | We try to model attributes after IDL attributes (see the link below for
-- the difference between content attributes and IDL attributes). That means
-- we don't treat 'attributeValue' as a simple string, but instead explicitly
-- differentiate between the different types (String, Bool, Int, URL etc).
--
-- https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes

data Attribute
    = AVAL !Text !AttributeValue
    | AEVL !EventListener
    | ASTY !Style
    | AREF !Ref

instance ToJSON Attribute where
    toJSON (AVAL name value)    = toJSON ("AVAL" :: Text, name, value)
    toJSON (AEVL eventListener) = toJSON ("AEVL" :: Text, eventListener)
    toJSON (ASTY style)         = toJSON ("ASTY" :: Text, style)
    toJSON (AREF ref)           = toJSON ("AREF" :: Text, ref)

instance FromJSON Attribute where
    parseJSON v = do
        (name, value) <- parseJSON v
        pure $ AVAL name value



lookupEventListener :: FID -> [Attribute] -> Maybe EventListener
lookupEventListener fid attrs = case attrs of
    [] -> Nothing
    (AEVL el@(EventListener _ fe):xs) -> if f1Id fe == fid
        then Just el
        else lookupEventListener fid xs
    (_:xs) -> lookupEventListener fid xs


-- $attributeValueConstructors
-- These constructors are here for convenience. You are encouraged to use
-- these functions instead of the 'Attribute' and 'AttributeValue'
-- constructors.

boolAttribute :: Text -> Bool -> Attribute
boolAttribute name value = AVAL name (AVBool value)

stringAttribute :: Text -> Text -> Attribute
stringAttribute name value = AVAL name (AVString value)

intAttribute :: Text -> Int -> Attribute
intAttribute name value = AVAL name (AVInt value)

doubleAttribute :: Text -> Double -> Attribute
doubleAttribute name value = AVAL name (AVDouble value)


eventListenerAttribute :: EventListener -> Attribute
eventListenerAttribute = AEVL


styleAttribute :: Style -> Attribute
styleAttribute = ASTY


refAttribute :: Ref -> Attribute
refAttribute = AREF
