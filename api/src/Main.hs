{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Api

import Network.Wai.Handler.Warp (runSettings, defaultSettings, setPort, setHost)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (defaultConnectionOptions)

main :: IO ()
main = runSettings settings $ websocketsOr defaultConnectionOptions appWS appHTTP
  where
    settings = setPort 8081 $ setHost "0.0.0.0" defaultSettings