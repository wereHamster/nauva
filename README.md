[![license](https://img.shields.io/github/license/wereHamster/nauva.svg?style=flat-square)]() [![GitHub issues](https://img.shields.io/github/issues/wereHamster/nauva.svg?style=flat-square&label=GitHub+–+Issues)]() [![Code Climate](https://img.shields.io/codeclimate/issues/github/wereHamster/nauva.svg?style=flat-square&label=Code+Climate+–+Issues)]()

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

For best experience use macOS. Better support for other platforms (Linux, Windows)
will follow later.

First install [stack], then:

    git clone https://github.com/wereHamster/nauva.git
    cd nauva
    ./bin/dev template/app

A browser should automatically open. If not then open [http://localhost:8000](http://localhost:8000)
manually. Note: if port 8000 is already occupied the server will pick the next
free port. See in the output which port the server has picked.

Now edit the template app source file (product/template/app/dev/src/Main.hs),
save and observe how the UI instantly reloads to reflect your changes.


## Documentation

Haddock documentation of the core packages is available [here][nvdocs].
It is automatically built from the current source (`master` branch).


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

 - **bin/**
 - **pkg/**
   - **hs/** - Main packages which are written in Haskell
     - **nauva/** - The core Nauva package
     - **nauva-css/** - CSS DSL
     - **nauva-catalog/** - Catalog (interactive development and documentation environment)
     - **nauva-dev-server/** - A shell to run an application in develompent mode
       (server-side rendering, instant reload etc.).
     - **navua-native/** - JavaScript bindings to run an application in the browser.
     - **nauvad/** - Heavily modified [ghcid] with integrated HTTP / WebSocket server.
 - **product/**
   - **playground/** - Really ugly application which I use to test new features, performance etc.
   - **varna/** - Port of one of my personal applications, a somewhat more realistic example of a real-world application.
   - **nauva/** - For now just the catalog for some of the UI components used by nauva itself.


### Products

A product is a collection of one or more applications which share some common code
(business logic, theme, colors, typefaces, UI components etc). Each application is
in its own subfolder and is split into yet more folders. Each of these folders has
one specific purpose, contains one Cabal file which defines a single executable.

In the following example we have a product which consists of an application (**app**)
and a website (**website**) which share common code. Furthermore, both **app** and
**website** have a **dev** variant which is what you can run with **bin/dev**
(to work on the code locally). The **app** can be compiled into **native**
(JavaScript) code, while the **website** can produce a **static** version which
you can upload to your HTTP server.

 - **shared/** - Any code which is independent of the platform where the application will run.
   Must be free of GHCJS-specific code, and not use libraries which can not be compiled with
   GHCJS. Nor must it use libraries which can not run in a JavaScript environment.
 - **app/**
   - **dev/** - Project which runs the app using **nauvad** / **nauva-dev-server**.
   - **native/** - Project which compiles the app into a native JavaScript file.
 - **website/**
   - **dev/** - Project which runs the website using **nauvad** / **nauva-dev-server**.
   - **static/** - Project which compiles the website into static output (HTML + CSS files).


[react]: https://facebook.github.io/react
[GHCJS]: https://github.com/ghcjs/ghcjs
[stack]: https://github.com/commercialhaskell/stack
[ghcid]: https://github.com/ndmitchell/ghcid
[nvdocs]: https://storage.googleapis.com/nvdocs/latest/index.html
