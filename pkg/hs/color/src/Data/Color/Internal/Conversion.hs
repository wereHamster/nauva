{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes      #-}

module Data.Color.Internal.Conversion where


import Control.Lens

import Data.Color.Internal.Types



------------------------------------------------------------------------------
-- CIExyY

toCIExyY :: Iso' Color CIExyY
toCIExyY = iso sa bt
  where
    sa Color{..} = CIExyY x y (1 - x - y)
      where
        x = cX / (cX + cY + cZ)
        y = cY / (cX + cY + cZ)

    bt (CIExyY x y cY) = Color{..}
      where
        cX = (cY / y) * x
        cZ = (cY / y) * (1 - x - y)



------------------------------------------------------------------------------
-- CIELAB

toCIELAB :: Chromaticity -> Iso' Color CIELAB
toCIELAB white = iso sa bt
  where
    Color {cX = xn, cY = yn, cZ = zn} = mkColor white 1
    rho = 6 / 29

    sa :: Color -> CIELAB
    sa Color{..} = CIELAB (116 * fy - 16) (500 * (fx - fy)) (200 * (fy - fz))
      where
        (fx, fy, fz) = (f (cX/xn), f (cY/yn), f (cZ/zn))

        f t = if t > (rho ** 3) then t ** (1/3) else (t / (3 * (rho ** 2))) + 4 / 29


    bt (CIELAB l a b) = Color x y z
      where
        l' = (l + 16) / 116

        x = xn * f (l' + (a / 500))
        y = yn * f (l'            )
        z = zn * f (l' - (b / 200))

        f t = if t > rho then t ** 3 else 3 * (rho ** 2) * (t - 4 / 29)



------------------------------------------------------------------------------
-- SRGB

toSRGB :: Iso' Color SRGB
toSRGB = iso sa bt
  where
    a = 0.055

    sa Color{..} = SRGB r g b
      where
        r' =  3.2404542 * cX - 1.5371385 * cY - 0.4985314 * cZ
        g' = -0.9692660 * cX + 1.8760108 * cY + 0.0415560 * cZ
        b' =  0.0556434 * cX - 0.2040259 * cY + 1.0572252 * cZ

        r = f r'
        g = f g'
        b = f b'

        f t = if t < 0.0031308 then 12.92 * t else (a + 1) * (t ** (1 / 2.4)) - a;

    bt (SRGB r g b) = Color{..}
      where
        r' = f r
        g' = f g
        b' = f b

        cX = 0.4124564 * r' + 0.3575761 * g' + 0.1804375 * b'
        cY = 0.2126729 * r' + 0.7151522 * g' + 0.0721750 * b'
        cZ = 0.0193339 * r' + 0.1191920 * g' + 0.9503041 * b'

        f t = if t <= 0.04045 then t / 12.92 else ((t + a) / (a + 1)) ** 2.4
