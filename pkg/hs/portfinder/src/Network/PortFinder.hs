{-# LANGUAGE ScopedTypeVariables #-}

module Network.PortFinder
    ( findPort
    ) where


import Control.Exception
import Network.Socket



-- | Return a free port to which the application can bind a socket.
--
-- Throws an error if it can't find a free port.

findPort :: IO PortNumber
findPort = do
    let hints = defaultHints { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV], addrSocketType = Stream }
    addr:_ <- getAddrInfo (Just hints) (Just "0.0.0.0") (Just "9000")
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

    go 99 sock (addrAddress addr)

  where
    go :: Int -> Socket -> SockAddr -> IO PortNumber
    go 0 _ _ = error "findPort: exhausted"
    go i sock addr = try sock addr `catch` \(e :: IOException) -> do
        -- Bummer. Try the next port.
        let sockAddr@(SockAddrInet port a) = addr
        go (i - 1) sock (SockAddrInet (port + 1) a)

    try :: Socket -> SockAddr -> IO PortNumber
    try sock sockAddr@(SockAddrInet port addr) = do
        -- Try to bind the socket the the address. This will throw
        -- an 'IOException' in the following common cases:
        --
        --  - EADDRINUSE: The port is occupied (bound to an exinstig socket).
        --  - EACCESS: Port is priviledged (<1024) and user is not root.
        bind sock sockAddr

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
