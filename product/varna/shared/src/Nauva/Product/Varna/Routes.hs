module Nauva.Product.Varna.Routes
    ( Route(..)
    ) where


import Data.Text (Text)



data Route
    = HomeR
    | BatteryR !Text


-- match :: [(Route, Element)] -> Maybe Element
