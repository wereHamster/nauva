{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DeriveGeneric     #-}

module Nauva.Product.Nauva.Book.App
    ( bookApp
    , catalogPages
    ) where


import Data.Text
import Data.Monoid
import qualified Data.Aeson as A

import Nauva.App
import Nauva.Catalog
import Nauva.Catalog.TH



bookApp :: App
bookApp = App
    { rootElement = catalog . CatalogProps "Nauva" catalogPages
    }


catalogPages :: [Page]
catalogPages =
    [ PLeaf Leaf
        { leafHref = "/"
        , leafTitle = "Introduction"
        , leafElement = $(catalogPageFromFile
            "../../../../../../../../docs/book/introduction.md")
        }
    , PLeaf Leaf
        { leafHref = "/getting-started"
        , leafTitle = "Getting started"
        , leafElement = $(catalogPageFromFile
            "../../../../../../../../docs/book/getting-started.md")
        }
    , PLeaf Leaf
        { leafHref = "/markup"
        , leafTitle = "Markup"
        , leafElement = $(catalogPageFromFile
            "../../../../../../../../docs/book/markup.md")
        }
    , PLeaf Leaf
        { leafHref = "/styles"
        , leafTitle = "Styles"
        , leafElement = $(catalogPageFromFile
            "../../../../../../../../docs/book/styles.md")
        }
    , PLeaf Leaf
        { leafHref = "/thunks"
        , leafTitle = "Thunks"
        , leafElement = $(catalogPageFromFile
            "../../../../../../../../docs/book/thunks.md")
        }
   , PLeaf Leaf
        { leafHref = "/components"
        , leafTitle = "Components"
        , leafElement = $(catalogPageFromFile
            "../../../../../../../../docs/book/components.md")
        }
    ]


data State = State
    { numberOfClicks :: Int
    }

data Action
    = Clicked

instance Value Action where
    parseValue _ = pure Clicked


initialState :: State
initialState = State
    { numberOfClicks = 0
    }

updateState :: Action -> State -> State
updateState Clicked State{..} = State { numberOfClicks = numberOfClicks + 1 }


renderCounter :: State -> Element
renderCounter State{..} = div_
    [ button_ [onClick_ onClickHandler] [str_ "Click Me!"]
    , span_ [str_ ("Clicked " <> pack (show numberOfClicks) <> " times" :: Text)]
    ]
  where
    onClickHandler :: F1 MouseEvent (EventHandler Action)
    onClickHandler = eventHandler $ \_ -> do
        stopPropagation
        action $ value0E "Clicked"


counterComponent :: Component () () State Action
counterComponent = createComponent $ \componentId -> Component
    { componentId = componentId
    , componentDisplayName = "Counter"
    , initialComponentState = \_ -> pure (initialState, [], [])
    , componentEventListeners = const []
    , componentHooks = emptyHooks
    , processLifecycleEvent = \() _ s -> (s, [])
    , receiveProps = \_ s -> pure (s, [], [])
    , update = \a _ s -> (updateState a s, [])
    , renderComponent = \_ -> renderCounter
    , componentSnapshot = \_ -> A.object []
    , restoreComponent = \_ s -> Right (s, [])
    }
