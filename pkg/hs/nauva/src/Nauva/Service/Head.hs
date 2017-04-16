{-# LANGUAGE OverloadedStrings #-}

module Nauva.Service.Head
    ( HeadH(..)
    , constHead
    ) where


import           Data.Aeson as A

import           Control.Concurrent.STM

import           Nauva.View



data HeadH = HeadH
    { hElements :: TVar [Element]
      -- ^ The list of elements which are to be shown in the <head>.

    , hReplace :: [Element] -> IO ()
      -- ^ Replace the current set of 'Elements' with new ones.
    }



constHeadComponent :: Component (HeadH, [Element], Element) () () ()
constHeadComponent = createComponent $ \cId -> Component
    { componentId = cId
    , componentDisplayName = "constHeadComponent"
    , initialComponentState = \(headH, headElements, _) -> pure ((), [], [updateHead headH headElements])
    , componentEventListeners = \_ -> []
    , componentHooks = emptyHooks
    , processLifecycleEvent = \() _ s -> (s, [])
    , receiveProps = \_ s -> pure (s, [], [])
    , update = \() (headH, headElements, _) s -> (s, [updateHead headH headElements])
    , renderComponent = \(_, _, el) _ -> el
    , componentSnapshot = \_ -> A.object []
    , restoreComponent = \_ s -> Right (s, [])
    }
  where
    updateHead :: HeadH -> [Element] -> IO (Maybe ())
    updateHead headH headElements = do
        hReplace headH headElements
        pure Nothing

constHead :: HeadH -> [Element] -> Element -> Element
constHead headH headElements el =
    component_ constHeadComponent (headH, headElements, el)
