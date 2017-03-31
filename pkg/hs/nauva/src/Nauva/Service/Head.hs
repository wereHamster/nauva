module Nauva.Service.Head
    ( HeadH(..)
    ) where


import           Control.Concurrent.STM

import           Nauva.View



data HeadH = HeadH
    { hElements :: TVar [Element]
      -- ^ The list of elements which are to be shown in the <head>.

    , hReplace :: [Element] -> IO ()
      -- ^ Replace the current set of 'Elements' with new ones.
    }
