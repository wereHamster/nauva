module Main (main) where


import           Nauva.Server
import           Nauva.Catalog
import           Nauva.Product.Template.Catalog (catalogPages)



main :: IO ()
main = runServer $ Config
    { cElement = \routerH -> catalog $ CatalogProps routerH catalogPages
    , cHead = mempty
    }
