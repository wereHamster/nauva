{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Nauva.Product.Nauva.Book.App
    ( bookApp
    , catalogPages
    ) where


import Data.Text

import Nauva.App
import Nauva.Catalog
import Nauva.Catalog.TH



bookApp :: App
bookApp = App
    { rootElement = catalog . CatalogProps catalogPages
    }


catalogPages :: [Page]
catalogPages =
    [ PLeaf Leaf
        { leafHref = "/"
        , leafTitle = "Introduction"
        , leafElement = $(catalogPageFromFile
            "../../../../../../../../docs/book/introduction.md")
        }
    , PDirectory Directory
        { directoryTitle = "Getting Started"
        , directoryChildren =
            [ Leaf
                { leafHref = "/getting-started"
                , leafTitle = "Source"
                , leafElement = $(catalogPageFromFile
                    "../../../../../../../../docs/book/getting-started/source.md")
                }
            , Leaf
                { leafHref = "/getting-started/views"
                , leafTitle = "Views"
                , leafElement = $(catalogPageFromFile
                    "../../../../../../../../docs/book/getting-started/views.md")
                }
            ]
        }
    ]
