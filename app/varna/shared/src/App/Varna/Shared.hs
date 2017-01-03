{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module App.Varna.Shared
    ( root

    , batteryCard
    ) where


import qualified Data.Aeson as A

import           Nauva.Internal.Types
import           Nauva.View

import           App.Varna.Element.Card

import           Prelude hiding (rem)



root :: Element
root = component_ rootComponent ()

rootComponent :: Component () () () ()
rootComponent = createComponent $ \componentId -> Component
    { componentId = componentId
    , componentDisplayName = "Root"
    , initialComponentState = \_ -> pure ((), [])
    , componentEventListeners = \_ -> []
    , componentHooks = emptyHooks
    , processLifecycleEvent = \_ s -> (s, [])
    , receiveProps = \_ s -> pure (s, [], [])
    , update = \_ _ _ -> ((), [])
    , renderComponent = renderComponent
    , componentSnapshot = \_ -> A.Null
    , restoreComponent = \_ s -> Right (s, [])
    }
  where
    renderComponent _ _ = div_ [style_ rootStyle] $
        [ navbar
        , batteries
            [ batteryCard
                [ batteryCardBodyParagraph True [str_ "This looks like a fresh battery. Congratulations on your purchase."]
                , batteryCardBodyPrimaryButton "charge battery"
                , batteryCardBodySecondaryButton "discharge battery"
                ]
            , batteryCard []
            , batteryCard []
            , batteryCard []
            , batteryCard []
            , batteryCard []
            , batteryCard []
            , batteryCard []
            , batteryCard []
            , batteryCard []
            , batteryCard []
            ]
        ]
      where
        rootStyle :: Style
        rootStyle = mkStyle $ do
            height (vh 100)
            display flex
            flexDirection column
            fontFamily "museo-slab, serif"




navbar :: Element
navbar = div_ [style_ rootStyle]
    [ div_ [style_ navbarItemStyle] [ str_ "Home" ]
    , div_ [style_ navbarItemStyle] [ str_ "Create Battery" ]
    ]
  where
    rootStyle :: Style
    rootStyle = mkStyle $ do
        display flex
        flexDirection row
        height (rem 3)
        backgroundColor "rgb(126, 120, 3)"
        color "rgb(228, 228, 238)"
        fontSize (rem 1.8)
        lineHeight (rem 3)
        flexShrink "0"

    navbarItemStyle :: Style
    navbarItemStyle = mkStyle $ do
        padding (rem 0) (rem 0.5)
        cursor pointer

        onHover $ do
            backgroundColor "#999"


batteries :: [Element] -> Element
batteries = div_ [style_ rootStyle]
  where
    rootStyle = mkStyle $ do
        marginTop (rem 1)
        display flex
        flexDirection row
        flexWrap wrap
