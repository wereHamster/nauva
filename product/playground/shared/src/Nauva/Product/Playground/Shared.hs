{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Nauva.Product.Playground.Shared
    ( playgroundApp
    ) where


import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Aeson as A
import           Data.Monoid

import           System.Random

import           GHC.Generics (Generic)

import           Nauva.App
import           Nauva.View



playgroundApp :: App
playgroundApp = App
    { rootElement = \_ -> playgroundRootElement 1
    }



-- | The root element of the application. It takes one input (an 'Int'), and
-- contains both 'Thunk's and 'Component's, to demonstrate that they all work
-- as expected (ie. 'Thunk's are forced and 'Components' retain their state).
playgroundRootElement :: Int -> Element
playgroundRootElement i = div_ [style_ rootStyle :: Attribute] $
    [ str_ $ "App Generation " <> (T.pack $ show i)
    , br_ []
    , thunk_ thunk (i `div` 2)
    , br_ []
    , component_ component (i `div` 3)
    , br_ []
    , div_ [style_ canvasContainerStyle] $
        [ component_ canvas ()
        , component_ canvas ()
        ]
    ]

  where
    rootStyle :: Style
    rootStyle = mkStyle $ do
        height (vh 100)
        display flex
        flexDirection column

    canvasContainerStyle :: Style
    canvasContainerStyle = mkStyle $ do
        flex "1"
        display flex
        flexDirection row


thunk :: Thunk Int
thunk = createThunk $ \thunkId -> Thunk thunkId "thunk" (==) $ \i ->
    str_ $ "Thunk " <> (T.pack $ show i)


data Action = DoThis | DoThat | DoClick Text | DoChange Text
    deriving (Generic)

instance Value Action where
    parseValue v = do
        list <- A.parseJSON v
        case list of
            (t:xs) -> do
                ctag <- A.parseJSON t
                case ctag :: Text of
                    "DoClick" -> do
                        case xs of
                            [a] -> DoClick <$> A.parseJSON a
                            _ -> fail "Action:DoClick"
                    "DoChange" -> do
                        case xs of
                            [a] -> DoChange <$> A.parseJSON a
                            _ -> fail "Action:DoChange"
                    _ -> fail "Action"
            _      -> fail "Action"

component :: Component Int () (Int, Text) Action
component = createComponent $ \componentId -> Component
    { componentId = componentId
    , componentDisplayName = "app-display-name"
    , initialComponentState = \i -> pure ((i, ""), [], [])
    , componentEventListeners = \_ -> []
    , componentHooks = constHooks
    , receiveProps = receiveProps'
    , renderComponent = view
    , update = update'
    , processLifecycleEvent = \_ _ s -> (s, [])
    , componentSnapshot = A.toJSON
    , restoreComponent = \v _ -> case A.fromJSON v of
        A.Error e -> Left e
        A.Success s' -> Right (s', [])
    }

  where
    update' DoThat _ s = (s, [pure $ Just DoThis])
    update' DoThis _ s = (s, [])
    update' (DoClick t) _ (i, _) = ((i, t), [pure $ Just DoThis])
    update' (DoChange t) _ (i, _) = ((i, t), [pure $ Just DoThis])

    receiveProps' p (_, t) = pure ((p, t), [], [pure $ Just DoThat])

    view :: Int -> (Int, Text) -> Element
    view _ (i, t) = span_
        [ str_ $ "Component " <> (T.pack $ show i)
        , button_ [style_ buttonStyle, onClick_ onClickHandler, value_ ("TheButtonValue" :: Text)] [str_ "Click Me!"]
        , input_ [onChange_ onChangeHandler, value_ t]
        , str_ t
        ]

    buttonStyle :: Style
    buttonStyle = mkStyle $ do
        fontFamily_ $ do
            src "local('GeosansLight-NMS')"

        outline none
        border none
        fontSize (px 18)

        padding (px 4) (px 8) (px 5)

        backgroundColor "black"
        color "white"

        onHover $ do
            backgroundColor "green"

        media "max-width: 400px" $ do
            color "red"


targetE :: Exp MouseEvent -> Exp t
targetE = getE (litE ("target" :: Text))

targetValueE :: Exp MouseEvent -> Exp Text
targetValueE = getE (litE ("value" :: Text)) . targetE

onClickHandler :: F1 MouseEvent (EventHandler Action)
onClickHandler = eventHandler $ \ev -> do
    stopPropagation
    action $ value1E "DoClick" (targetValueE ev)

onChangeHandler :: FE MouseEvent Action
onChangeHandler = eventHandler $ \ev -> do
    action $ value1E "DoChange" (targetValueE ev)


data CanvasA
    = Mouse Float Float
    | SetSize Float Float
    deriving (Generic)

instance A.FromJSON CanvasA
instance A.ToJSON CanvasA

instance Value CanvasA where
    parseValue v = do
        list <- A.parseJSON v
        case list of
            (t:xs) -> do
                ctag <- A.parseJSON t
                case ctag :: Text of
                    "Mouse" -> do
                        case xs of
                            [a, b] -> Mouse <$> A.parseJSON a <*> A.parseJSON b
                            _ -> fail "CanvasA:Mouse"
                    "SetSize" -> do
                        case xs of
                            [a, b] -> SetSize <$> A.parseJSON a <*> A.parseJSON b
                            _ -> fail "CanvasA:SetSize"
                    _ -> fail "CanvasA"
            _      -> fail "CanvasA"

data CanvasS = CanvasS
    { _csMousePosition :: (Float, Float)
    , _csRefKey :: RefKey
    , _csOnResize :: FE MouseEvent CanvasA
    , _csSize :: Maybe (Int, Int)
    }

instance A.ToJSON CanvasS where
    toJSON (CanvasS mouse rk _ size) = A.toJSON (mouse, rk, size)

canvas :: Component () () CanvasS CanvasA
canvas = createComponent $ \componentId -> Component
    { componentId = componentId
    , componentDisplayName = "Canvas"
    , initialComponentState = \_ ->
        let refKey = mkRefKey
        in pure (CanvasS (0,0) refKey (onResizeHandler refKey) Nothing, [], [])
    , componentEventListeners = \(CanvasS _ _ onResizeH _) ->
        [onResize onResizeH]
    , componentHooks = emptyHooks
    , receiveProps = receiveProps'
    , renderComponent = view
    , update = update'
    , processLifecycleEvent = \_ _ s -> (s, [])
    , componentSnapshot = A.toJSON
    , restoreComponent = \_ s -> Right (s, [])
    }

  where
    update' :: CanvasA -> () -> CanvasS -> (CanvasS, [IO (Maybe CanvasA)])
    update' (Mouse x y)   _ (CanvasS _ refKey onResizeH s) = (CanvasS (x, y - 100) refKey onResizeH s, [])
    update' (SetSize w h) _ (CanvasS m refKey onResizeH _) = (CanvasS m refKey onResizeH (Just (floor w, floor h)), [])

    receiveProps' () s = pure (s, [], [])

    numCircles = 100

    circles (width, height) n = flip map [1..numCircles] $ \i ->
        let (x, stdGen) = next (mkStdGen $ n + i)
            (y, _     ) = next stdGen
        in circle_
            [ r_ (6 :: Int)
            , cx_ (x `mod` width)
            , cy_ (y `mod` height)
            , fill_ ("black" :: Text)
            ] []

    -- attach = refHandler $ \componentH element -> do
    --     storeRef componentH (litE "svg") element
    --     action $ ....

    attach :: FRA el Action
    attach = createF $ \fId -> F1 fId $ \element ->
        refHandlerE (justE $ value2E "SetSize" (elementWidth element) (elementHeight element))

    detach :: FRD Action
    detach = createF $ \fId -> F0 fId $ refHandlerE nothingE

    view :: () -> CanvasS -> Element
    view _ (CanvasS (x,y) refKey _ s) = div_ [style_ style, ref_ (Ref (Just refKey) attach detach)] $
        case s of
            Nothing -> []
            Just (w,h) -> [svg ((x,y), (w, h))]

    style :: Style
    style = mkStyle $ do
        flex "1"
        display flex

    svg ((x,y), (width, height)) = svg_
        [style_ svgStyle, onMouseMove_ onMouseMoveHandler, width_ width, height_ height, className_ ("canvas" :: Text)] $
        [ rect_ [x_ (0 :: Int), y_ (0 :: Int), width_ width, height_ height, fill_ ("#DDD" :: Text)] []
        , circle_ [r_ (12 :: Int), cx_ (T.pack $ show x), cy_ (T.pack $ show y), fill_ ("magenta" :: Text)] []
        ] ++ circles (width, height) (floor x)

    svgStyle :: Style
    svgStyle = mkStyle $ do
        flex "1"
        display block


onResizeHandler :: RefKey -> FE MouseEvent CanvasA
onResizeHandler (RefKey refKey) = eventHandler $ \_ -> do
    action $ value2E "SetSize" (elementWidth element) (elementHeight element)
  where
    element = derefE refKey

onMouseMoveHandler :: FE MouseEvent CanvasA
onMouseMoveHandler = eventHandler $ \ev -> do
    action $ value2E "Mouse" (clientXE ev) (clientYE ev)

clientXE :: Exp MouseEvent -> Exp Float
clientXE = getE (litE "clientX" :: Exp Text)

clientYE :: Exp MouseEvent -> Exp Float
clientYE = getE (litE "clientY" :: Exp Text)


elementWidth :: Exp a -> Exp Float
elementWidth element = domRectWidth $ getBoundingClientRect element

elementHeight :: Exp a -> Exp Float
elementHeight element = domRectHeight $ getBoundingClientRect element
