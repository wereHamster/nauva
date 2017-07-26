The `Nauva` Haskell framework is an attempt to provide a solid foundation for
building UI applications which use the W3C DOM as the underlying presentation
technology.

Nauva borrows many concepts from [React][react] - such as the virtual DOM,
stateful components, unidirectional data binding - and implements them in Haskell.

Applications written in Nauva are portable between server and client. The same
code can be compiled and run on the server, but can also compiled by [GHCJS]
and shipped to a web browser where it runs as a JavaScript application.

[react]: https://facebook.github.io/react/
[GHCJS]: https://github.com/ghcjs/ghcjs
