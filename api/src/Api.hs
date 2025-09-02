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
import qualified Data.Text as T
import Data.Text (Text)
import Data.Char (isSpace)
import GHC.Generics
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.ByteString.Lazy.Char8()

import Control.Concurrent.STM (atomically, newTBQueue, readTBQueue)
import Control.Concurrent.Async (withAsync)
import Control.Monad (forever, void)

import Commitment (fetchDataFrom)
import Types ()
import Threading (safePrint)

------------------------------------------------------------
-- Incoming JSON
------------------------------------------------------------
data ClientMessage = ClientMessage { url :: String }
  deriving (Show, Generic)

instance FromJSON ClientMessage

encodeValue :: ToJSON a => Text -> a -> BL.ByteString
encodeValue label val = encode $ object ["type" .= label, "data" .= val]

encodeError :: Text -> BL.ByteString
encodeError msg = encode $ object ["type" .= ("error" :: Text), "message" .= msg]

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

------------------------------------------------------------
-- 📡 WebSocket Handler
------------------------------------------------------------
appWS :: ServerApp
appWS pendingConn = do
  conn <- acceptRequest pendingConn

  msg <- receiveData conn
  case eitherDecode (BL.fromStrict msg) of
    Left _ -> do
      sendTextData conn $ encodeError "Invalid JSON format. Expected: {\"url\": \"...\"}"
      sendClose conn ("Bad input" :: Text)

    Right (ClientMessage repoUrl) -> do
      safePrint $ "Processing URL (WS): " ++ repoUrl
      notifier <- atomically $ newTBQueue 1000

      -- Stream text_update messages while job runs
      withAsync (forever $ do
          update <- atomically $ readTBQueue notifier
          sendTextData conn (BL.toStrict $ encodeValue "text_update" update)
        ) $ \_ -> do

        result <- fetchDataFrom (trim repoUrl) notifier
        case result of
          Right repoData -> sendTextData conn (BL.toStrict $ encodeValue "value" repoData)
          Left errmsg    -> do
            safePrint $ "Encountered error: " ++ errmsg
            sendTextData conn (BL.toStrict $ encodeValue "text_update" errmsg)
            sendTextData conn (BL.toStrict $ encodeError (T.pack ("err: " ++ errmsg)))

      sendClose conn ("Done" :: Text)

------------------------------------------------------------
-- 🌐 HTTP Fallback Handler (POST only)
------------------------------------------------------------
fallback :: Application
fallback req respond =
  if requestMethod req /= methodPost
    then respond $ responseLBS status405 [("Content-Type", "text/plain")] "Method Not Allowed"
    else do
      body <- strictRequestBody req
      case eitherDecode body of
        Left _ ->
          respond $ responseLBS status400 [("Content-Type", "application/json")]
                    $ encodeError "Invalid JSON format. Expected: {\"url\": \"...\"}"

        Right (ClientMessage repoUrl) -> do
          safePrint $ "Processing URL (HTTP): " ++ repoUrl
          notifier <- atomically $ newTBQueue 1000

          -- Discard updates, but ensure worker cleaned up
          withAsync (forever $ void (atomically (readTBQueue notifier))) $ \_ -> do
            result <- fetchDataFrom (trim repoUrl) notifier
            case result of
              Right repoData ->
                respond $ responseLBS status200 [("Content-Type", "application/json")]
                          $ encodeValue "value" repoData
              Left errmsg -> do
                safePrint $ "Encountered error:\n" ++ errmsg
                respond $ responseLBS status500 [("Content-Type", "application/json")]
                          $ encodeError (T.pack ("err: " ++ errmsg))

------------------------------------------------------------
-- Exported HTTP app
------------------------------------------------------------
appHTTP :: Application
appHTTP = fallback
