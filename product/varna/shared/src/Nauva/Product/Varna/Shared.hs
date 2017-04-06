{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Nauva.Product.Varna.Shared
    ( root

    , batteryCard
    ) where


import qualified Data.Aeson as A

import           Nauva.View

import           Nauva.Service.Head

import           Nauva.Product.Varna.Element.Card

import           Prelude hiding (rem)



root :: HeadH -> Element
root = component_ rootComponent

rootComponent :: Component HeadH () () ()
rootComponent = createComponent $ \cId -> Component
    { componentId = cId
    , componentDisplayName = "Root"
    , initialComponentState = \p -> pure ((), [], [updateHead p])
    , componentEventListeners = \_ -> []
    , componentHooks = emptyHooks
    , processLifecycleEvent = \_ _ s -> (s, [])
    , receiveProps = \_ s -> pure (s, [], [])
    , update = \_ p _ -> ((), [updateHead p])
    , renderComponent = render
    , componentSnapshot = \_ -> A.Null
    , restoreComponent = \_ s -> Right (s, [])
    }
  where
    updateHead :: HeadH -> IO (Maybe ())
    updateHead headH = do
        hReplace headH
            [ style_ [str_ "*,*::before,*::after{box-sizing:inherit}body{margin:0;box-sizing:border-box}"]
            , title_ [str_ "Varna"]
            ]
        pure Nothing

    render _ _ = div_ [style_ rootStyle] $
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
    , null_
    , div_ [style_ navbarItemStyle] [ str_ "Account" ]
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
