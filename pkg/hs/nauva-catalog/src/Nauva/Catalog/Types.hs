{-# LANGUAGE OverloadedStrings #-}

module Nauva.Catalog.Types
    ( Page(..)
    , Leaf(..)
    , Directory(..)

    , onlyLeaves
    ) where


import           Data.Text          (Text)
import           Data.Monoid

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


-- | Return a flat list of 'Leaf' values. Useful when you want to enumerate all
-- (directly referenceable) URLs inside the catalog.
onlyLeaves :: [Page] -> [Leaf]
onlyLeaves []     = []
onlyLeaves (x:xs) = case x of
    PLeaf leaf   -> leaf : onlyLeaves xs
    PDirectory d -> directoryChildren d <> onlyLeaves xs
