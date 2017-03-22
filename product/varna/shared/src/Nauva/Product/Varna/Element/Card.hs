{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Nauva.Product.Varna.Element.Card
    ( batteryCard

    , batteryCardBodyParagraph
    , batteryCardBodyPrimaryButton
    , batteryCardBodySecondaryButton

    , catalogPage
    ) where


import           Data.Text (Text)

import           Control.Monad

import           Nauva.Internal.Types
import           Nauva.View

import           Nauva.Catalog.TH (nauvaCatalogPage)

import           Prelude hiding (rem)



batteryCard :: [Element] -> Element
batteryCard body = div_ [style_ rootStyle] $ [batteryCardBody body]
  where
    rootStyle :: Style
    rootStyle = mkStyle $ do
        margin (rem 0.5)
        width "calc(100vw - 16px)"
        height (rem 12)

        display flex

        -- min: 254px -> 260px
        media "(min-width: 552px) and (max-width: 827px)" $ do
            width "calc((100vw - 32px) / 2)"

        media "(min-width: 828px) and (max-width: 1167px)" $ do
            width "calc((100vw - 48px) / 3)"

        media "(min-width: 1168px)" $ do
            width "calc((100vw - 64px) / 4)"


batteryCardBody :: [Element] -> Element
batteryCardBody body = div_ [style_ style] $ [batteryTileHeader] ++ body
  where
    style = mkStyle $ do
        flex "1"
        display flex
        flexDirection column
        backgroundColor "rgb(240, 239, 244)"


batteryTileHeader :: Element
batteryTileHeader = div_ [style_ rootStyle]
    [ indicator
    , batId
    , meta
    ]
  where
    rootStyle :: Style
    rootStyle = mkStyle $ do
        display flex
        flexDirection row

        backgroundColor "rgb(45, 48, 57)"
        color "rgb(236, 225, 233)"

        backgroundColor "#1f633c"

    indicatorStyle :: Style
    indicatorStyle = mkStyle $ do
        width (rem 1.5)
        height (rem 3)

        backgroundColor "rgb(158, 156, 156)"
        color "rgb(236, 225, 233)"

        lineHeight (rem 3)
        fontSize (rem 2.2)
        textAlign center

        backgroundColor "#28b262"

    indicator = div_ [style_ indicatorStyle] []

    batIdStyle :: Style
    batIdStyle = mkStyle $ do
        flex "1"
        fontSize (rem 2.2)
        lineHeight (rem 3)
        marginLeft (rem 0.6)

    batId = div_ [style_ batIdStyle] [str_ "XXX"]

    metaStyle :: Style
    metaStyle = mkStyle $ do
        display flex
        flexDirection column
        textAlign right
        marginRight (rem 0.3)

    meta = div_ [style_ metaStyle]
        [ metaRow "3S / 1500mAh"
        , metaRow "4.198V"
        ]


    metaRow s = div_ [style_ metaRowStyle] [str_ s]

    metaRowStyle :: Style
    metaRowStyle = mkStyle $ do
        height (rem 1.5)
        lineHeight (rem 1.5)


-- batteryCardBody :: [Element] -> Element
-- batteryCardBody = div_ [style_ rootStyle]
--   where
--     rootStyle = mkStyle $ do
--         flex "1"
--         display flex
--         justifyContent center
--         flexDirection column
--         alignItems center

batteryCardBodyParagraph :: Bool -> [Element] -> Element
batteryCardBodyParagraph centered = div_ [style_ rootStyle]
  where
    rootStyle = mkStyle $ do
        margin (rem 0.0) (rem 0)
        padding (rem 0) (rem 1)
        flex "1"
        when centered $ do
            display flex
            flexDirection column
            justifyContent center
            textAlign center

batteryCardBodyPrimaryButton :: Text -> Element
batteryCardBodyPrimaryButton label = div_ [style_ rootStyle] [str_ label]
  where
    rootStyle = mkStyle $ do
        -- button
        display inlineBlock
        padding (rem 0.2) (rem 0.5) (rem 0.1)
        backgroundColor "#ddd"
        textAlign center
        cursor pointer


        -- battery-card-body-button
        backgroundColor "rgb(226, 206, 226)"
        color "rgb(37, 37, 32)"

        fontSize (rem 1.5)
        textTransform uppercase

        padding (rem 0.5) (rem 1) (rem 0.4)

        onHover $ do
            backgroundColor "rgb(202, 174, 202)"

batteryCardBodySecondaryButton :: Text -> Element
batteryCardBodySecondaryButton label = div_ [style_ rootStyle] [str_ label]
  where
    rootStyle = mkStyle $ do
        -- button
        display inlineBlock
        padding (rem 0.2) (rem 0.5) (rem 0.1)
        backgroundColor "#ddd"
        textAlign center
        cursor pointer


        -- battery-card-body-button
        backgroundColor "rgb(226, 206, 226)"
        color "rgb(37, 37, 32)"

        fontSize (rem 1.5)
        textTransform uppercase

        padding (rem 0.5) (rem 1) (rem 0.4)


        -- battery-card-body-button-secondary
        backgroundColor "rgb(136, 113, 136)"
        color "white"
        fontSize (rem 0.75)

        onHover $ do
            backgroundColor "rgb(112, 86, 112)"


catalogPage :: Element
catalogPage = [nauvaCatalogPage|
# Overview

A `batteryCardBody` is the container for the body elements. It is epected to be placed
in a flex container and expands to the size to its container (`flex:1`).

The minimum dimensions are `254px` wide and `12rem` tall, but ideally `20rem` by `14rem`.

```nauva
div_ [style_ $ mkStyle (display flex >> flexDirection row >> justifyContent "space-between")]
    [ div_ [style_ $ mkStyle (display flex >> width "254px" >> height "12rem")] [batteryCardBody []]
    , div_ [style_ $ mkStyle (display flex >> width "20rem" >> height "14em")] [batteryCardBody []]
    ]
```

# Body Elements

You can place any of these elements into the card body:

 - `batteryCardBodyParagraph`
 - `batteryCardBodyPrimaryButton`
 - `batteryCardBodySecondaryButton`

```nauva
div_ [style_ $ mkStyle (display flex >> width "20rem" >> height "14rem")]
    [ batteryCardBody
        [ batteryCardBodyParagraph True [str_ "This looks like a fresh battery. Congratulations on your purchase."]
        , batteryCardBodyPrimaryButton "charge battery"
        , batteryCardBodySecondaryButton "discharge battery"
        ]
    ]
```
|]
