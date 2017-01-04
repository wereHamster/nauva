{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Nauva.Service.Router
    ( RouterH(..)
    , Location(..)

    , LinkProps(..)
    , link
    ) where


import           Data.Text  (Text)
import qualified Data.Aeson as A

import           Control.Concurrent.STM

import           Nauva.Internal.Types
import           Nauva.Internal.Events
import           Nauva.View
import           Nauva.NJS



data RouterH = RouterH
    { hLocation :: (TVar Location, TChan Location)
      -- ^ The current 'Location' and a (broadcast) 'TChan' which can
      -- be subscribed to in order to receive notifications when the location
      -- changes.

    , hPush :: Text -> IO ()
      -- ^ Push a new URL to the router stack.
    }


data Location = Location
    { locPathname :: !Text
    }



-------------------------------------------------------------------------------
-- link

data LinkProps = LinkProps
    { routerH :: !RouterH
      -- ^ The router handle provides the callbacks.
    , href :: !Text
      -- ^ The path which shall be pushed to the router history when the element
      -- is clicked.
    , element :: !Element
      -- ^ The element to which a 'onClick' handler is attached.
    }

link :: LinkProps -> Element
link = component_ linkComponent



linkComponent :: Component LinkProps () () ()
linkComponent = createComponent $ \componentId -> Component
    { componentId = componentId
    , componentDisplayName = "Link"
    , initialComponentState = \_ -> pure ((), [])
    , componentEventListeners = \_ -> []
    , componentHooks = emptyHooks
    , processLifecycleEvent = \() s -> (s, [])
    , receiveProps = \_ _ -> pure ((), [], [])
    , update = \() p s -> (s, [clickEffect p])
    , renderComponent = \(LinkProps {..}) _ -> with element [onClick_ onClickHandler]
    , componentSnapshot = \_ -> A.object []
    , restoreComponent = \_ s -> Right (s, [])
    }
  where
    onClickHandler :: F1 MouseEvent (EventHandler ())
    onClickHandler = eventHandler $ \_ -> do
        preventDefault
        action $ value0E $ njsCon0 "()" ()

    clickEffect :: LinkProps -> IO (Maybe ())
    clickEffect (LinkProps {..}) = do
        hPush routerH href
        pure Nothing
