{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Main (main) where


import Control.Concurrent (forkIO)
import Control.Exception (bracket)
import System.Environment (getEnvironment, setEnv, unsetEnv)
import Control.Concurrent (getNumCapabilities)
import GHC.Conc (getNumProcessors, setNumCapabilities)

import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets.Connection
  ( ConnectionOptions(..), defaultConnectionOptions )
import Network.Wai.Handler.Warp
  ( runSettings, defaultSettings, setPort, setHost, Settings
  , runSettingsSocket
  )









import Api
import Commitment
import Threading

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

-- | Initialize the runtime to use all logical cores
initializeRuntime :: IO ()
initializeRuntime = do
    cores <- getNumProcessors
    setNumCapabilities cores
    cap <- getNumCapabilities

    -- initialise the thread pools used for parsing + running commands
    let t1 = parsingPool
    let t2 = commandPool

    -- worker thread pools should be ready for use
    safePrint $ "number of cores available for use: " ++ show cores
    safePrint $ "number of cores available for parsingPool: " ++ show (numWorkers parsingPool)
    safePrint $ "number of cores available for commandPool: " ++ show (numWorkers commandPool)

    pure ()

main :: IO ()
main = do
    init <- initializeRuntime
    awaitEnvironmentClean <- cleanEnvironment

    -- Enable permessage-deflate (RSV1 frames allowed)
    let opts = defaultConnectionOptions  -- no connectionCompression
    let app  = websocketsOr opts appWS appHTTP


    -- On Windows: only TCP
    runSettings tcpSettings app








-- TCP Settings
tcpSettings :: Settings
tcpSettings = setPort 8081 $ setHost "0.0.0.0" defaultSettings


