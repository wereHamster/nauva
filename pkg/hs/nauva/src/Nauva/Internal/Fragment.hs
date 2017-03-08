module Nauva.Internal.Fragment where


import Control.Monad.Writer.Lazy

import Nauva.DOM
import Nauva.Internal.Types




-- | A 'Fragment' is a list of 'Element's. Often when rendering we want to
-- incrementally build the children list of a 'ENode'. Doing this with the
-- normal list notation is cumbersome.
--
-- Furthermore, a 'Fragment' allows us to pass multiple 'Element's as if they
-- were a unit, without having to wrap them in a node (eg. @"span"@
-- or @"div"@).
--
-- There are downsides though to using 'Fragment's. TK: explain.

newtype Fragment = Fragment { unFragment :: [Element] }

instance Monoid Fragment where
    mempty = Fragment []
    (Fragment a) `mappend` (Fragment b) = Fragment (a <> b)


singleton :: Element -> Fragment
singleton x = Fragment [x]


type FragmentM = Writer Fragment ()

-- | Execute a 'FragmentM' and simplify the result by merging adjacent
-- 'EString' elements.
execFragmentM :: FragmentM -> [Element]
execFragmentM = simplify . unFragment . execWriter
  where
    simplify []                       = []
    simplify (EText a : EText b : xs) = simplify (EText (a <> b) : xs)
    simplify (x:xs)                   = x : simplify xs


nodeFromFragment :: Tag -> [Attribute] -> FragmentM -> Element
nodeFromFragment tag attrs fm = ENode tag attrs $ execFragmentM fm
