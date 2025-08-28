{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Api
import Control.Concurrent (forkIO)
import Control.Exception (bracket)

import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (defaultConnectionOptions)

import Network.Wai.Handler.Warp (runSettings, defaultSettings, setPort, setHost, Settings)
import Network.Wai.Handler.Warp (runSettingsSocket)
import Network.Socket
  ( socket, bind, listen, Socket, SockAddr(SockAddrUnix)
  , Family(AF_UNIX), SocketType(Stream)
  , close
  )

main :: IO ()
main = do
    let app = websocketsOr defaultConnectionOptions appWS appHTTP

    -- Run TCP server on 0.0.0.0:8081
    _ <- forkIO $ runSettings tcpSettings app

    -- Run Unix domain socket server on /tmp/haskell-ipc.sock
    bracket (setupUnixSocket "/tmp/haskell-ipc.sock") close $ \unixSock ->
        runSettingsSocket defaultSettings unixSock app

-- TCP Settings
tcpSettings :: Settings
tcpSettings = setPort 8081 $ setHost "0.0.0.0" defaultSettings

-- Helper to create a Unix domain socket
setupUnixSocket :: FilePath -> IO Socket
setupUnixSocket path = do
    sock <- socket AF_UNIX Stream 0
    bind sock (SockAddrUnix path)
    listen sock 1024
    return sock
