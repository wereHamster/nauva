{-# LANGUAGE ScopedTypeVariables #-}

module Network.PortFinder
    ( findPort
    ) where


import Control.Exception (IOException, catch)
import Network.Socket



-- | Return a free port to which the application can bind a socket.
--
-- Throws an error if it can't find a free port.

findPort :: PortNumber -> IO PortNumber
findPort basePort = do
    let hints = defaultHints { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV], addrSocketType = Stream }
    addr:_ <- getAddrInfo (Just hints) (Just "0.0.0.0") (Just $ show basePort)
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

    case addrAddress addr of
        (SockAddrInet port hostAddress) -> go 99 sock port hostAddress
        _                               -> error "findPort: not an inet addr"


  where
    go :: Int -> Socket -> PortNumber -> HostAddress -> IO PortNumber
    go 0 _ _ _ = error "findPort: exhausted"
    go i sock port hostAddress = try sock port hostAddress `catch` \(_ :: IOException) ->
        -- Bummer. Try the next port.
        go (i - 1) sock (port + 1) hostAddress

    try :: Socket -> PortNumber -> HostAddress -> IO PortNumber
    try sock port hostAddress = do
        -- Try to bind the socket the the address. This will throw
        -- an 'IOException' in the following common cases:
        --
        --  - EADDRINUSE: The port is occupied (bound to an exinstig socket).
        --  - EACCESS: Port is priviledged (<1024) and user is not root.
        bind sock (SockAddrInet port hostAddress)

        -- Verify that we can listen on the port. May not be necessary but
        -- it's a good, additional check to have.
        --
        -- Another useful check would be to try to connect to the socket,
        -- to verify that it's actually reachable.
        listen sock 5

        -- Alright, if we've reached this point without running into
        -- an exception, it means the port is available. Clean up so
        -- that the caller can actually bind to the socket himself.
        close sock

        pure port
