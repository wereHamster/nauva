# Nauva

> **Etymology:** nauva, from Quenya, "it will".

The `Nauva` Haskell library is an attempt to provide a solid foundation for
building UI applications which use the W3C DOM as the underlying presentation
technology.

Nauva borrows many concepts from [React][react] - such as the virtual DOM,
stateful components, unidirectional data binding - and implements them in Haskell.

Applications written in Nauva are portable between server and client. The same
code can be compiled and run on the server, but can also compiled by [GHCJS]
and shipped to a web browser where it runs as a JavaScript application.


## TLDR

Pick how you want to run the playground app:

### Compile once and run in the dev-server shell

    $ stack build
    $ stack exec nauva-example-playground-server

### Using ghcid for instant-reload experience

    $ stack install ghcid
    $ stack exec ghcid -- --command="stack ghci" --test=:main

And then open [http://localhost:8000](http://localhost:8000) in a web browser.

If you've chosen the *instant-realod* experience, the webpage will automatically
reload -- usually within a fraction of a second -- after you edit the App module
(examples/playground/App/src/Navua/Playground/App.hs).


## Focus and Goals

 - Implement as much as possible in Haskell, only use JavaScript where
   necessary for performance or convenience.
 - Provide a good developer experience. That includes server-side
   rendering and near-instant reload times during development (to
   match expectations of people coming from the webpack ecosystem).
 - Good performance. At the very least should be fast enough to render
   interactive SVG visualizations.


## Structure of the repository

This repository contains multiple packages and projects (ie. a monorepo) which
are all related to Nauva.

 - **pkg/** - All main packages
   - **hs/** - Main packages which are written in Haskell
     - **nauva/** - The core Nauva package
     - **navua-native/** - JavaScript bindings to run an application in the browser.
     - **nauva-dev-server/** - A shell to run an application in develompent mode
       (server-side rendering, instant reload etc.).
 - **examples/** - Examples
   - **playground/** - Used to experiment and play around with new features.


### Examples

All examples are structured in the same way. Each directory contains three
sub-directories. The reason is because we have two modes how we compile and run
the application (native / dev-server) and want to share code between the two.

 - **app/** - The application itself. It is important that this code is independent
   of where the application will run. Must be free of GHCJS-specific code, and not use
   libraries which can not be compiled with GHCJS.
 - **native/** - Project which compiles the app into a native JavaScript file.
 - **server/** - Project which runs the app using **nauva-dev-server**.


[react]: https://facebook.github.io/react
[GHCJS]: https://github.com/ghcjs/ghcjs
