{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

module Nauva.Product.Nauva.Catalog (catalogApp) where


import           Data.Color

import           Nauva.App

import           Nauva.Catalog
import           Nauva.Catalog.TH
import           Nauva.Catalog.Elements
import           Nauva.Catalog.Theme.Color
import           Nauva.Catalog.Theme.Typeface
import           Nauva.Catalog.Specimens.TypefaceSpecimen

import           Nauva.Product.Nauva.Element.Terminal as Terminal
import           Nauva.Product.Nauva.Element.Message  as Message



catalogApp :: App
catalogApp = App
    { rootElement = catalog . CatalogProps "Nauva" catalogPages
    }


catalogPages :: [Page]
catalogPages =
    [ PLeaf $ Leaf
        { leafHref = "/"
        , leafTitle = "Introduction"
        , leafElement = introductionPage
        }
    , PDirectory $ Directory
        { directoryTitle = "Theme"
        , directoryChildren =
            [ Leaf
                { leafHref = "/theme/colors"
                , leafTitle = "Colors"
                , leafElement = colorsPage
                }
            , Leaf
                { leafHref = "/theme/typefaces"
                , leafTitle = "Typefaces"
                , leafElement = typefacesPage
                }
            ]
        }
    , PDirectory $ Directory
        { directoryTitle = "Elements"
        , directoryChildren =
            [ Leaf
                { leafHref = "/elements/terminal"
                , leafTitle = "Terminal"
                , leafElement = Terminal.catalogPage
                }
            , Leaf
                { leafHref = "/elements/message"
                , leafTitle = "Message"
                , leafElement = Message.catalogPage
                }
            ]
        }
    ]


introductionPage :: Element
introductionPage = [nauvaCatalogPage|
# Welcome to the Nauva catalog
|]


colorsPage :: Element
colorsPage = [nauvaCatalogPage|
```element
colorGroup $ ColorGroup
    { cgLabel = "Black"
    , cgCells =
        [ ColorCell
            { ccLabel = "100"
            , ccLuminance = 90
            , ccValue = Just $ ColorCellValue
                { csvName = "black100"
                , csvColor = mkColor (Chromaticity 0.305 0.329) 0.76
                }
            }
        , ColorCell
            { ccLabel = "200"
            , ccLuminance = 80
            , ccValue = Just $ ColorCellValue
                { csvName = "black200"
                , csvColor = mkColor (Chromaticity 0.305 0.329) 0.56
                }
            }
        , ColorCell
            { ccLabel = "500"
            , ccLuminance = 50
            , ccValue = Just $ ColorCellValue
                { csvName = "black500"
                , csvColor = mkColor (Chromaticity 0.288 0.291) 0.185
                }
            }
        , ColorCell
            { ccLabel = "900"
            , ccLuminance = 10
            , ccValue = Just $ ColorCellValue
                { csvName = "black900"
                , csvColor = blackColor
                }
            }
        ]
    }
```
|]


typefacesPage :: Element
typefacesPage = [nauvaCatalogPage|
# Headings

The heading typefaces are used in the catalog for for the main page header (h1)
and headings inside the page content (h2 – h4).

```element
typefaceSpecimen' h2Typeface
```

```element
typefaceSpecimen' h3Typeface
```

```element
typefaceSpecimen' h4Typeface
```

# Copy

```element
typefaceSpecimen "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum." paragraphTypeface
```

```element
typefaceSpecimen "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum." blockquoteTypeface
```

# System

The system typefaces are used in some elements rendered by nauvad.

```element
typefaceSpecimen "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum." system14Typeface
```

# Monospace

Monospace fonts are used to show terminal output and code blocks.

```element
typefaceSpecimen "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum." mono12Typeface
```

```element
typefaceSpecimen "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum." mono14Typeface
```

|]
