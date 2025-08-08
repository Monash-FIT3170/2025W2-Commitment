{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Api (
  appWS,
  appHTTP,
  fallback
) where

import Network.Wai
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets
  ( acceptRequest
  , receiveData
  , sendTextData
  , sendClose
  , defaultConnectionOptions
  , ServerApp
  )

import Network.HTTP.Types
  ( status200
  , status400
  , status405
  , status500
  , methodPost
  )

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8() 

import Control.Concurrent.STM (atomically, newTBQueue, readTBQueue)
import Control.Concurrent (forkIO)
import Control.Monad (forever, void)

import Commitment (fetchDataFrom)
import Types ()

-- Incoming JSON from JS
data ClientMessage = ClientMessage { url :: String }
  deriving (Show, Generic)

instance FromJSON ClientMessage

-- Outgoing structured response
encodeValue :: ToJSON a => Text -> a -> BL.ByteString
encodeValue label val = encode $ object ["type" .= label, "data" .= val]

encodeError :: Text -> BL.ByteString
encodeError msg = encode $ object ["type" .= ("error" :: Text), "message" .= msg]

------------------------------------------------------------
-- 📡 WebSocket Handler
------------------------------------------------------------
appWS :: ServerApp
appWS pendingConn = do
  conn <- acceptRequest pendingConn
  putStrLn "WebSocket client connected"

  -- Step 1: Wait for { "url": "..." } from client
  msg <- receiveData conn
  case eitherDecode (BL.fromStrict msg) of
    Left err -> do
      putStrLn $ "Invalid JSON: " ++ err
      sendTextData conn $ encodeError "Invalid JSON format. Expected: {\"url\": \"...\"}"
      sendClose conn ("Bad input" :: Text)

    Right (ClientMessage repoUrl) -> do
      putStrLn $ "Processing URL (WS): " ++ repoUrl
      notifier <- atomically $ newTBQueue 1000

      -- Stream text_update messages
      _ <- forkIO $ forever $ do
        update <- atomically $ readTBQueue notifier
        sendTextData conn $ BL.toStrict $ encodeValue "text_update" update

      -- Run the actual job
      result <- fetchDataFrom repoUrl notifier
      case result of
        Just repoData -> sendTextData conn $ BL.toStrict $ encodeValue "value" repoData
        Nothing       -> sendTextData conn $ BL.toStrict $ encodeError "No repository data found."

      sendClose conn ("Done" :: Text)

------------------------------------------------------------
-- 🌐 HTTP Fallback Handler (POST only)
------------------------------------------------------------
fallback :: Application
fallback req respond = do
  if requestMethod req /= methodPost
    then respond $ responseLBS status405 [("Content-Type", "text/plain")] "Method Not Allowed"
    else do
      body <- strictRequestBody req
      case eitherDecode body of
        Left err -> do
          putStrLn $ "Invalid JSON (HTTP): " ++ err
          respond $ responseLBS status400 [("Content-Type", "application/json")] $ encodeError "Invalid JSON format. Expected: {\"url\": \"...\"}"

        Right (ClientMessage repoUrl) -> do
          putStrLn $ "Processing URL (HTTP): " ++ repoUrl
          notifier <- atomically $ newTBQueue 1000

          -- We discard updates for HTTP; could log or save
          _ <- forkIO $ forever $ void (atomically (readTBQueue notifier))

          result <- fetchDataFrom repoUrl notifier
          case result of
            Just repoData ->
              respond $ responseLBS status200 [("Content-Type", "application/json")] $
                encodeValue "value" repoData
            Nothing ->
              respond $ responseLBS status500 [("Content-Type", "application/json")] $
                encodeError "No repository data found."

appHTTP :: Application
appHTTP = websocketsOr defaultConnectionOptions appWS fallback