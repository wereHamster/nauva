module Main (main) where


import           Nauva.Server
import           Nauva.Catalog
import           Nauva.Product.Template.Catalog (catalogPages)



main :: IO ()
main = runServer $ Config
    { cPort = 8080
    , cElement = \routerH -> catalog $ CatalogProps routerH catalogPages
    , cPublicDir = Nothing
    , cHead = mempty
    }
