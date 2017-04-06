module Main (main) where


import           Nauva.Server
import           Nauva.Catalog
import           Nauva.Product.Template.Catalog (catalogPages)



main :: IO ()
main = runServer $ Config
    { cElement = \headH routerH -> catalog $ CatalogProps headH routerH catalogPages
    , cHead = mempty
    }
