module Data.Color.Illuminant
    ( d55
    , d65
    ) where


import Data.Color.Internal.Types


d55 :: Chromaticity
d55 = Chromaticity 0.33242 0.34743

d65 :: Chromaticity
d65 = Chromaticity 0.31271 0.32902
