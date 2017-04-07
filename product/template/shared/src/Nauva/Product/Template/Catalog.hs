{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE DeriveGeneric         #-}

module Nauva.Product.Template.Catalog (catalogPages) where


import           Data.Text
import qualified Data.Text as T
import           Data.Monoid
import qualified Data.Aeson as A
import           Data.Tagged

import           GHC.Generics (Generic)

import           Nauva.Internal.Types

import           Nauva.Catalog.Types
import           Nauva.Catalog.TH

import           Nauva.View



catalogPages :: [Page]
catalogPages =
    [ PLeaf Leaf
        { leafHref = "/"
        , leafTitle = "Introduction"
        , leafElement = introductionPage
        }
    , PLeaf Leaf
        { leafHref = "/input"
        , leafTitle = "Input"
        , leafElement = inputPage
        }
    ]


introductionPage :: Element
introductionPage = [nauvaCatalogPage|
# Welcome to the Template catalog
|]






data InputProps = InputProps
    { ipIsValid :: Bool
    , ipValue :: Text
    , ipOnChange :: FE MouseEvent Action
    }

input :: InputProps -> Element
input InputProps{..} = div_
    [ style_ rootStyle ] $

    [ input_ [style_ inputStyle, value_ ipValue, onChange_ ipOnChange]
    , validityIndicator
    ]

  where
    validityIndicator = if ipIsValid
        then span_ [style_ $ mkStyle $ marginLeft (px 4) >> color "green" >> fontWeight "bold"] [str_ "OK"]
        else span_ [style_ $ mkStyle $ marginLeft (px 4) >> color "red" >> fontWeight "bold"] [str_ "←"]

    rootStyle = mkStyle $ do
        display flex
        alignItems "center"

    inputStyle = mkStyle $ do
        display block
        width "100%"
        border none
        padding "4px" "7px" "3px"
        fontSize "16px"
        outline none

        onHover $ do
            border "1px solid blue"

        if not ipIsValid
            then do
                border "1px solid red"
            else do
                border "1px solid transparent"





inputPage :: Element
inputPage = [nauvaCatalogPage|
# `input`

TK: Describe this!

```nauva
span: 3
noSource: true
title: valid
---
input $ InputProps
    True
    "Bacon navigating Inverse proactive"
    (eventHandler $ \_ -> pure ())
```

```nauva
span: 3
noSource: true
title: invalid
---
input $ InputProps
    False
    "Central deliver Awesome Granite Pizza Investment Account"
    (eventHandler $ \_ -> pure ())
```



# `input` wrapped in a stateful component

```nauva
noSource: true
---
component_ validatedInputComponent ()
```


|]











isValidValue :: Text -> Bool
isValidValue x = T.length x < 3











data ValidatedInputState = ValidatedInputState
    { visValue :: Text
    }







data Action = ChangeValue !Text
    deriving (Generic)







instance A.FromJSON Action
instance A.ToJSON Action

instance Value Action where
    parseValue v = do
        list <- A.parseJSON $ unTagged v
        case list of
            (t:xs) -> do
                ctag <- A.parseJSON t
                case ctag :: Text of
                    "ChangeValue" -> do
                        case xs of
                            [a] -> ChangeValue <$> A.parseJSON a
                            _ -> fail "Action:ChangeValue"
                    _ -> fail "Action"
            _ -> fail "Action"







validatedInputComponent :: Component () () ValidatedInputState Action
validatedInputComponent = createComponent $ \cId -> Component
    { componentId = cId
    , componentDisplayName = "ValidatedInputComponent"
    , componentEventListeners = \_ -> []
    , componentHooks = emptyHooks
    , processLifecycleEvent = \_ _ s -> (s, [])
    , receiveProps = \_ s -> pure (s, [], [])


    , componentSnapshot = \_ -> A.Null
    , restoreComponent = \_ s -> Right (s, [])





    , initialComponentState = \() -> pure (ValidatedInputState "", [], [])

    , update = \a _ s -> case a of
        (ChangeValue t) -> (s { visValue = t }, [])

    , renderComponent = \_ (ValidatedInputState{..}) ->
        input $ InputProps
            (isValidValue visValue)
            visValue
            onChangeHandler

    }







  where
    onChangeHandler :: FE MouseEvent Action
    onChangeHandler = eventHandler $ \ev -> do
        action $ value1E conChangeValue (targetValueE ev)

    targetValueE :: Exp MouseEvent -> Exp Text
    targetValueE = getE (litE ("value" :: Text)) . getE (litE ("target" :: Text))

    conChangeValue :: Con1 Text Action
    conChangeValue = njsCon1 "ChangeValue" ChangeValue










validatedInput :: Element
validatedInput = component_ validatedInputComponent ()



{-

# `input` wrapped in a stateful component

```nauva
noSource: true
---
div_ [component_ validatedInputComponent ()]
```



-}