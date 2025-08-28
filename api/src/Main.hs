{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Api
import Control.Concurrent (forkIO)

import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (defaultConnectionOptions)

import Network.Wai.Handler.Warp (runSettings, defaultSettings, setPort, setHost)
import Network.Wai.Handler.Warp.Unix (runSettingsUnix, defaultUnixSettings, setUnixSocket)

main :: IO ()
main = do
  let app = websocketsOr defaultConnectionOptions appWS appHTTP

  -- Run TCP server on 0.0.0.0:8081
  _ <- forkIO $ runSettings tcpSettings app

  -- Run Unix socket server on /tmp/haskell-ipc.sock
  runSettingsUnix unixSettings app

  where
    tcpSettings  = setPort 8081 $ setHost "0.0.0.0" defaultSettings
    unixSettings = setUnixSocket "/tmp/haskell-ipc.sock" defaultUnixSettings