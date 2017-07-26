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