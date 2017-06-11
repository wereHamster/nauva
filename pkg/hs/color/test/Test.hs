{-# LANGUAGE OverloadedStrings #-}

module Main where


import Test.Hspec

import Control.Lens

import Data.Color.Illuminant
import Data.Color.Internal.Types
import Data.Color.Internal.Conversion



-- D65 at 0% and 100% luminance.
whiteD65_0, whiteD65_1 :: Color
whiteD65_0 = mkColor d65 0
whiteD65_1 = mkColor d65 1

-- sRGB primaries
sRGB_r, sRGB_g, sRGB_b :: Color
(sRGB_r, sRGB_g, sRGB_b) =
    ( mkColor (Chromaticity 0.6400 0.3300) 0.2126
    , mkColor (Chromaticity 0.3000 0.6000) 0.7152
    , mkColor (Chromaticity 0.1500 0.0600) 0.0722
    )

_unColorV :: Getter (ColorV a) (a, a, a)
_unColorV = to unColorV

within :: (Num a, Ord a) => a -> a -> a -> Bool
within eps a b = abs (a - b) < eps


main :: IO ()
main = hspec $ parallel $ do
    describe "colorLuminance" $ do
        it "luminance of sRGB (124,124,124) should be 20% (18% gray card)" $
            mkSRGB8 (124,124,124) ^.re toSRGB ^. colorLuminance `shouldSatisfy` within 0.005 0.2
        it "luminance of sRGB (128,128,128) should be 21.40% (50% sRGB brightness)" $
            mkSRGB8 (128,128,128) ^.re toSRGB ^. colorLuminance `shouldSatisfy` within 0.005 0.214
        it "luminance of sRGB (188,188,188) should be 50% (middle gray as defined by absolute whiteness)" $
            mkSRGB8 (188,188,188) ^.re toSRGB ^. colorLuminance `shouldSatisfy` within 0.005 0.5

    describe "CIE LAB" $ do
        describe "toCIELAB" $ do
            it "should convert D65 at 0% luminance to L=0" $
                whiteD65_0 ^. toCIELAB d65 ^. cvCIELAB ^. _unColorV ^. _1 `shouldBe` 0
            it "should convert D65 at 100% luminance to L=100" $
                whiteD65_1 ^. toCIELAB d65 ^. cvCIELAB ^. _unColorV ^. _1 `shouldBe` 100

        describe "L=50" $
            it "should convert to sRGB (119,119,119)" $
                CIELAB 50 0 0 ^.re (toCIELAB d65) ^. toSRGB ^. cvSRGB8 `shouldBe` ColorV (119,119,119)

    describe "sRGB" $
        describe "toSRGB" $
            it "should convert D65 at 0% luminance to (0,0,0)" $
                whiteD65_0 ^. toSRGB ^. cvSRGB8 `shouldBe` ColorV (0,0,0)
            it "should convert D65 at 100% luminance to (255,255,255)" $
                whiteD65_1 ^. toSRGB ^. cvSRGB8 `shouldBe` ColorV (255,255,255)

            describe "primaries" $ do
                it "should convert the red primary to (255,0,0)" $
                    sRGB_r ^. toSRGB ^. cvSRGB8 `shouldBe` ColorV (255,0,0)
                it "should convert the green primary to (0,255,0)" $
                    sRGB_g ^. toSRGB ^. cvSRGB8 `shouldBe` ColorV (0,255,0)
                it "should convert the blue primary to (0,0,255)" $
                    sRGB_b ^. toSRGB ^. cvSRGB8 `shouldBe` ColorV (0,0,255)


    describe "regressions" $
        it "roundtrip from sRGB to CIE LAB and back" $
            let rgb = (44,9,103)
                ColorV (l, a, b) = mkSRGB8 rgb ^.re toSRGB ^. toCIELAB d65 ^. cvCIELAB
            in CIELAB l a b ^.re (toCIELAB d65) ^. toSRGB ^. cvSRGB8 ^. _unColorV `shouldBe` rgb
