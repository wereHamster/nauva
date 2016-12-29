module Nauva.Service.Router
    ( RouterH(..)
    , Location(..)
    ) where


import Data.Text (Text)
import Control.Concurrent.STM



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
