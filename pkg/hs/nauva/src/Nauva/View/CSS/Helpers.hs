module Nauva.View.CSS.Helpers where

import qualified Data.Text as T

import           Nauva.Internal.Types
import           Nauva.View.CSS.Internal



vh :: Int -> CSSValue
vh n = CSSValue $ T.pack $ (show n) ++ "vh"
