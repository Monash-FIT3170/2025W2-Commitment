{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Main (main) where

import Api
import Control.Concurrent (forkIO)
import Control.Exception (bracket)
import System.Environment (getEnvironment, setEnv, unsetEnv)

import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets.Connection
  ( ConnectionOptions(..), defaultConnectionOptions )
import Network.Wai.Handler.Warp
  ( runSettings, defaultSettings, setPort, setHost, Settings
  , runSettingsSocket
  )

#if !defined(mingw32_HOST_OS)
import Network.Socket
  ( socket, bind, listen, Socket, SockAddr(SockAddrUnix)
  , Family(AF_UNIX), SocketType(Stream)
  , close
  )
#endif

cleanEnvironment :: IO ()
cleanEnvironment = do
    -- remove problematic variables
    unsetEnv "GIT_ASKPASS"
    unsetEnv "GCM_INTERACTIVE"

    -- inject variables to disable prompts
    setEnv "GIT_TERMINAL_PROMPT" "0"
    setEnv "SSH_AUTH_SOCK" "\\.\\pipe\\ssh-auth-sock"

    pure ()

socketPath :: String
socketPath = "/tmp/haskell-ipc.sock"

main :: IO ()
main = do
    awaitEnvironmentClean <- cleanEnvironment

    -- Enable permessage-deflate (RSV1 frames allowed)
    let opts = defaultConnectionOptions  -- no connectionCompression
    let app  = websocketsOr opts appWS appHTTP

#if defined(mingw32_HOST_OS)
    -- On Windows: only TCP
    runSettings tcpSettings app
#else
    -- On Unix: TCP + Unix socket
    _ <- forkIO $ runSettings tcpSettings app

    bracket (setupUnixSocket socketPath) close $ \unixSock ->
        runSettingsSocket defaultSettings unixSock app
#endif

-- TCP Settings
tcpSettings :: Settings
tcpSettings = setPort 8081 $ setHost "0.0.0.0" defaultSettings

#if !defined(mingw32_HOST_OS)
-- Helper to create a Unix domain socket
setupUnixSocket :: FilePath -> IO Socket
setupUnixSocket path = do
    sock <- socket AF_UNIX Stream 0
    bind sock (SockAddrUnix path)
    listen sock 1024
    return sock
#endif
