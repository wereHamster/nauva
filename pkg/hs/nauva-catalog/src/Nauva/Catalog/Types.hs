{-# LANGUAGE OverloadedStrings #-}

module Nauva.Catalog.Types
    ( Page(..)
    , Leaf(..)
    , Directory(..)
    ) where


import           Data.Text          (Text)

import           Nauva.Internal.Types




data Page
    = PLeaf !Leaf
    | PDirectory !Directory

data Leaf = Leaf
    { leafHref :: !Text
    , leafTitle :: !Text
    , leafElement :: !Element
    }

data Directory = Directory
    { directoryTitle :: !Text
    , directoryChildren :: ![Leaf]
    }
