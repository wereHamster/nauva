{-# LANGUAGE RecordWildCards #-}

module Data.Color.Internal.Types where


import Data.Word
import Control.Lens



-------------------------------------------------------------------------------
-- | A generic representation of a color as three values, usually numbers,
-- without giving them any particular meaning. It could be the RGB channels
-- of a SRGB or AdobeRGB color, XYZ tristimulus values, CIE xyY values
-- or something else.

newtype ColorV a = ColorV { unColorV :: (a, a, a) }
    deriving (Eq, Show)

instance Functor ColorV where
    fmap f (ColorV (a, b, c)) = ColorV (f a, f b, f c)



-------------------------------------------------------------------------------
-- | The x and y coordinates of CIE xyY.
--
-- The 'chromaX' and 'chromaY' components are usually referred to with @x@ and
-- @y@ symbols in mathematical formulas.
--
-- See https://en.wikipedia.org/wiki/Chromaticity

data Chromaticity = Chromaticity { chromaX :: !Double, chromaY :: !Double }
    deriving (Eq, Show)



-------------------------------------------------------------------------------
-- | Color is stored as CIE XYZ tristimulus values, normalized to @cY = 1@
-- being the brightest color.

data Color = Color { cX :: !Double, cY :: !Double, cZ :: !Double }
    deriving (Eq, Show)


-- | Construct a 'Color' from a 'Chromaticity' and the @Y@ tristimulus value.
mkColor :: Chromaticity -> Double -> Color
mkColor Chromaticity{..} cY = Color{..}
  where
    cX = (cY / chromaY) * chromaX
    cZ = (cY / chromaY) * (1 - chromaX - chromaY)


colorChromaticity :: Getter Color Chromaticity
colorChromaticity = to $ \Color{..} ->
    Chromaticity (cX / (cX + cY + cZ)) (cY / (cX + cY + cZ))

colorLuminance :: Getter Color Double
colorLuminance = to cY



------------------------------------------------------------------------------
-- | CIE xyY

data CIExyY = CIExyY !Double !Double !Double
    deriving (Eq, Show)

cvCIExyY :: Getter CIExyY (ColorV Double)
cvCIExyY = to $ \(CIExyY a b c) -> ColorV (a, b, c)



------------------------------------------------------------------------------
-- | CIE LAB
--
-- The L is normalized to [0..100].

data CIELAB = CIELAB !Double !Double !Double
    deriving (Eq, Show)

cvCIELAB :: Getter CIELAB (ColorV Double)
cvCIELAB = to $ \(CIELAB a b c) -> ColorV (a, b, c)



------------------------------------------------------------------------------
-- | sRGB
--
-- The RGB channels are normalized to [0..1].

data SRGB = SRGB !Double !Double !Double
    deriving (Eq, Show)

mkSRGB8 :: (Word8, Word8, Word8) -> SRGB
mkSRGB8 (r, g, b) = SRGB (f r) (f g) (f b)
  where f x = fromIntegral x / 255


cvSRGB :: Getter SRGB (ColorV Double)
cvSRGB = to $ \(SRGB a b c) -> ColorV (a, b, c)

-- | Convert a 'SRGB' color to a 'ColorV' of 'Word8's. Channel values which
-- are out of bounds (<0 or >1) are clamped.
cvSRGB8 :: Getter SRGB (ColorV Word8)
cvSRGB8 = to $ \(SRGB a b c) -> ColorV (f a, f b, f c)
  where
    f x = round $ fromIntegral (maxBound :: Word8) * clamp x
    clamp x
        | x < 0     = 0
        | x > 1     = 1
        | otherwise = x
