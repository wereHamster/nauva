Components are reusable, self-contained chunks of your application. Components
have a local state, can react to events, can issue IO actions and more.

A component is defined at the very least by the type of local state it manages,
the type of actions it can process, an initial state, and an update function.


# Counter

We'll start with a simple component: a counter which counts how many times
a button was clicked.

It is useful to define this logic separated from any Nauva-specific dependencies.
This makes it easy to reason just about the logic, and also makes testing easier.

### Types

First we defined our two types.

```
data State = State
    { numberOfClicks :: Int
    }

data Action
    = Clicked
```

### Initial state

Then we need an initial state.

```
initialState :: State
initialState = State
    { numberOfClicks: 0
    }
```

### Update function

```
updateState :: Action -> State -> State
updateState Clicked State{..} = State { numberOfClicks = numberOfClicks + 1 }
```

## View function

The second big piece is a function which renders the UI for this counter.

```
renderCounter :: State -> Element
renderCounter State{..} = div_ []
    [ button_
        [ value_ "Click Me!"
        ] []
    , div_ [] [str_ $ "Clicked " <> show numberOfClicks <> " times"]
    ]
```


## Creating our counter component

```
counterComponent :: Component () () State ()
counterComponent = createComponent $ \componentId -> Component
    { componentId = componentId
    , componentDisplayName = "Counter"
    , initialComponentState = \_ -> pure (initialState, [], [])
    , componentEventListeners = const []
    , componentHooks = emptyHooks
    , processLifecycleEvent = \() _ s -> (s, [])
    , receiveProps = \_ s -> pure (s, [], [])
    , update = \a _ s _ -> (updateState a s, [])
    , renderComponent = …
    , componentSnapshot = \_ -> A.object []
    , restoreComponent = \_ s -> Right (s, [])
    }
```

# Working example

```nauva
component_ counterComponent ()
```
