Sometimes you have parts of an application which rarely or never change, but
are expensive to render. For example the footer, or any part of the website
which the user is *not* currently interacting with. For these cases Nauva
offers `Thunks` as means to optimise these parts.

You can think of a `Thunk` as a pure function `p -> Element` and a predicate
`p -> p -> Bool`. If you try to render the same thunk, Nauva will use the
predicate to determine if the rendering should be skipped.

Once you have a `Thunk` defined, you can instantiate it with `thunk_`.


# simpleThunk

Use `simpleThunk` to create a `Thunk` if your `p` has an `Eq` instance.

```
expensiveFunction :: MyData -> Element
expensiveFunction mydata = …

expensiveFunctionThunk :: Thunk MyData
expensiveFunctionThunk = simpleThunk
    "expensiveFunctionThunk"
    expensiveFunction

optimisedFunction :: MyData -> Element
optimisedFunction = thunk_ expensiveFunctionThunk
```


# constElement

If you have an element which doesn't ever change, you can wrap your
element in `constElment` and it will optimise any subtree updates
away.

This is useful for static assets (SVG icons / logos) and larger non-interactive
things such as a footer.


```
footer :: Element
footer = constElement "footer" $ div_
    [
        …
    ]
```


# Next

[Components](/components).
