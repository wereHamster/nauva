The Nauva markup language is what you use to describe how the UI should look.
It is declarative, very similar to HTML and partly inspired by [React](https://facebook.github.io/react/) and [blaze-html](https://hackage.haskell.org/package/blaze-html).


# Imports

First we need to establishe some imports. Internally the code is split into
many separate modules. But to make it easier for users, all the functions are
re-exported through `Nauva.View`. It is recommended that you import the module
unqualified and without explicit import list.

    import Nauva.View


# HTML elements

Most HTML elements have an equivalent function in Nauva. The function name
is the same as the HTML tag but with an underscore suffix. The reason for
the suffix is so that there is no conflict between `div` from the `Prelude`
and the `<div>` HTML element (which is used very often).

We refer to these functions as *terms*. They are used to build the document
object model (DOM) tree. We use *term* because certain functions can appear
both as a tag (`<title>`) and as an attribute (`<a title="…">`) at the same
time. But more on that later.

There are a few special functions which don't correspond to a HTML tag. Two
important ones are `str_ :: Text -> Element` and `null_ :: Element`.


## Example

```nauva
div_ [str_ "This is a <div> element with a text within"]
```

The functions are overloaded and can take either one or two arguments (this
is possible through the use of multiple Haskell language extensions).

 - `div_ :: [Element] -> Element` – takes a list of children.
 - `div_ :: [Attribute] -> [Element] -> Element` – takes a list of attributes and then a list of childern.
 - `br_ :: [Attribute] -> Element` – takes a list of attributes.

```hint
To create an element with no children, you have to explicitly supply the type
signature. Without it the compiler gets confused.

Example: `let emptyDiv = (div_ [] :: Element)`
```


# HTML attributes

The same *terms* that are used to create HTML elements can also be used
to create attributes. Nauva can attach `Bool`, `Text`, `Int`, and `Double`
attributes directly to an element. Other attributes you have to manually
convert to one of those types first.


## Example

```nauva
a_ [href_ ("https://google.com" :: Text)] [str_ "This is a link"]
```

# Dual use of terms as tags and attributes

Remember when I said that there are terms which are use both as a tag
and attribute? `title_` is one such example. It can be used as a child
of `<head>` to set the page title, and also as an attribute on elements.


    -- As a child of <head>
    head_
        [ title_ [str_ "The Nauva Book"]
        ]

    -- As an attribute on <abbr>
    abbr_
        [title_ ("Glasgow Haskell Compiler" :: Text)]
        [str_ "GHC"]


# Next

There are two other important types of attributes that are attached to elments: styles and event handlers. More to that in the [next](/styles) chapter.
