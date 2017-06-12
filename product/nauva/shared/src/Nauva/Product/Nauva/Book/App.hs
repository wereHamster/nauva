{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DataKinds         #-}

module Nauva.Product.Nauva.Book.App
    ( bookApp
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
        , leafElement = introductionPage
        }
    , PDirectory Directory
        { directoryTitle = "Getting Started"
        , directoryChildren =
            [ Leaf
                { leafHref = "/getting-started"
                , leafTitle = "Source"
                , leafElement = gettingStartedPage
                }
            , Leaf
                { leafHref = "/getting-started/views"
                , leafTitle = "Views"
                , leafElement = gettingStartedViewsPage
                }
            ]
        }
    ]


introductionPage :: Element
introductionPage = [nauvaCatalogPage|
The `Nauva` Haskell framework is an attempt to provide a solid foundation for
building UI applications which use the W3C DOM as the underlying presentation
technology.

Nauva borrows many concepts from [React][react] - such as the virtual DOM,
stateful components, unidirectional data binding - and implements them in Haskell.

Applications written in Nauva are portable between server and client. The same
code can be compiled and run on the server, but can also compiled by [GHCJS]
and shipped to a web browser where it runs as a JavaScript application.
|]


gettingStartedPage :: Element
gettingStartedPage = [nauvaCatalogPage|
For best experience use macOS. Better support for other platforms (Linux, Windows)
will follow later.

First install [stack], then:

    git clone https://github.com/wereHamster/nauva.git
    cd nauva
    ./bin/nauva start template/app

A browser should automatically open. If not then open [http://localhost:8000](http://localhost:8000)
manually.

```hint
if port 8000 is already occupied the server will pick the next
free port. See in the output which port the server has picked.
```

Now edit the template app source file (product/template/app/dev/src/Main.hs),
save and observe how the UI instantly reloads to reflect your changes.
|]


gettingStartedViewsPage :: Element
gettingStartedViewsPage = [nauvaCatalogPage|
Views are pure functions which return an `Element`.

```hint
The equivalent to `Element` in React is a ReactElement – that is what you get when you
call the function `React.createElement(…)` or when you write eg. `<div>…</div>` when
using JSX.
```


## Imports

The module `Nauva.View` should give you all the functions you will ever need when dealing
with views.


## HTML Elements

Most HTML elements have an equivalent function in Nauva. The function name is the same as
the HTML tag but with an underscore suffix. See the following example:

```nauva
div_ [str_ "This is a DIV with a text"]
```

These functions take one or two arguments:

 - `div_ :: [Element] -> Element` – takes a list of children.
 - `div_ :: [Attribute] -> [Element] -> Element` – takes a list of attributes and then a list of childern.

```hint
To create an element with no children, you have to explicitly supply the type signature. Without
it the compiler gets confused.

Example: `let emptyDiv = (div_ [] :: Element)`
```

## Attributes

Simple attributes like `id`, `href` etc are converted directly to
attributes on the DOM element.

```nauva
a_ [href_ ("https://google.com" :: Text)] [str_ "This is a link"]
```

There are two other important types of attributes: styles and event handlers.

## Styling Elements

The recommended way to style the elments is through CSS-in-Haskell – styles defined
and constructed in Haskell that are attached to individual Elements.

```nauva
let bigRedTextStyle = mkStyle $ do
        color "red"
        fontSize (px 28)

in div_ [style_ bigRedTextStyle] [str_ "BIG RED TEXT"]
```

You can even use pseudo selectors, media queries etc.

```nauva
let bigRedTextWithHoverStyle = mkStyle $ do
        color "red"
        fontSize (px 28)
        cursor pointer

        onHover $ do
            color "green"

in div_ [style_ bigRedTextWithHoverStyle] [str_ "BIG RED TEXT TURNING GREEN ON HOVER"]
```

The CSS DSL uses the same terms as CSS IDL attributes, eg. `background-color`
becomes `backgroundColor`.


## Event handlers

Event handlers are `NJS` expressions that are attached to `Elements` and executed
when an event is dispatched to the `Element`.

```nauva
let onClickHandler :: F1 MouseEvent (EventHandler Int)
    onClickHandler = eventHandler $ \_ -> do
        stopPropagation
        action $ value0E $ njsCon0 "1" 1

in button_ [onClick_ onClickHandler] [str_ "Click Me!"]
```

|]

