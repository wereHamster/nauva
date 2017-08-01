In traditional UI frameworks you author and manage styling information
separately from markup (eg. in external CSS files). This approach
requires significant effort on the side of developers to keep the two
in sync. People have tried to solve this by using convention (BEM,
OOCSS, SCMACSS, SUITCSS, Atomic etc) or tooling (CSS modules +
webpack).

In Nauva you attach styling information directly to elements because
the two are often tightly coupled. And *if* you ever need to make
an element customisable or themeable, simply make the theme an
additional function argument, just like you would in any programming
language!


# Define styles

First you need to define a style. This is done with the `mkStyle`
function and a do block where you declare the style in a CSS-like DSL.

The CSS DSL uses the same terms as CSS IDL attributes, eg. `background-color`
becomes `backgroundColor`. Unlike the HTML terms there is no underscore
suffix. This makes the syntax very approachable to people who already know
CSS.

Like the HTML terms, the CSS DSL terms are overloaded. They can appear
both in place of the property or value. For example `flex` can be either
a property (`flex "1"`) or a value (`display flex`).

If for some reason a property or value is not defined, you have a very
easy way to define your own ones. Thanks to the `IsString` instance
you can simply use a string!

## Example

```
bigRedTextStyle :: Style
bigRedTextStyle = mkStyle $ do
    fontSize (px 28)

    -- Custom value for the 'color' property.
    color "red"

    -- Custom property and value.
    "-webkit-font-smoothing" "subpixel-antialiased"
```


# Attaching style to an element

Use `style_` to convert a `Style` to an `Attribute`.

```nauva
let bigRedTextStyle = mkStyle $ do
        color "red"
        fontSize (px 28)

in div_ [style_ bigRedTextStyle] [str_ "BIG RED TEXT"]
```

# Pseudo selectors, pseudo elements, and media queries

The CSS DSL supports pseudo selectors, pseudo elements, and media queries.
A limited number of combinators exist (`onHover`, `onActive`, `firstChild`
etc). You can also nest them.

```nauva
let bigRedTextWithHoverStyle = mkStyle $ do
        color "red"
        fontSize (px 28)
        cursor pointer

        onHover $ do
            color "green"

        media "(min-width: 1000px)" $ do
            color "blue"

            onHover $ do
                color "yellow"

in div_ [style_ bigRedTextWithHoverStyle] [str_ "BIG RED TEXT TURNING GREEN ON HOVER"]
```

```hint
The semantics of multiple uses of these combinators is not formally defined. Use
with caution!
```


# Next

The [next](/thunks) chapter explains how to use thunks to optimize performance.


## Event handlers

Event handlers are `NJS` expressions that are attached to `Elements` and executed
when an event is dispatched to the `Element`.

```nauva
let onClickHandler :: F1 MouseEvent Int
    onClickHandler = mkF1 ("ev", "MouseEvent")
        "ev.stopPropagation(); return [1]"

in button_ [onClick_ onClickHandler] [str_ "Click Me!"]
```
