{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Command (
  Command(..),
  CommandResult(..),
  getParsableStringFromCmd,
  defaultSuccess,
  defaultFail,
  defaultStdFail,
  logData,
  doNotLogData,
  executeCommand,
  deleteDirectoryIfExists
) where

import System.Exit (ExitCode(..))
import System.IO
import Control.Monad (when)
import Control.Concurrent.STM (TBQueue)
import Control.Exception (evaluate)
import System.Directory (doesDirectoryExist)
import System.Process
import Control.DeepSeq (force)

import Threading

data Command = Command
  { command    :: String
  , onSuccess  :: String -> String -> String
  , onFail     :: String -> String -> String
  , onStdFail  :: String -> String -> String -> String
  , shouldLog  :: Bool
  }

data CommandResult = CommandResult
  { result   :: String
  , errorMsg :: Maybe String
  , stdError :: Maybe String
  } deriving (Show, Eq)

getParsableStringFromCmd :: CommandResult -> String
getParsableStringFromCmd (CommandResult _ (Just err) _) = err
getParsableStringFromCmd (CommandResult r _ (Just se))  = if null r then se else r
getParsableStringFromCmd (CommandResult r _ _)          = r

defaultSuccess :: String -> String -> String
defaultSuccess c output = "Command succeeded:\n" ++ c ++ "\nOutput:\n" ++ output

defaultFail :: String -> String -> String
defaultFail c e = "Command:\n" ++ c ++ "\nError:\n" ++ e

defaultStdFail :: String -> String -> String -> String
defaultStdFail c _ se = "Command:\n" ++ c ++ "\nError:\n" ++ se

logData :: Command
logData = Command "" defaultSuccess defaultFail defaultStdFail True

doNotLogData :: Command
doNotLogData = logData { shouldLog = False }

executeCommand :: TBQueue String -> FilePath -> Command -> IO CommandResult
executeCommand notifier filepath f = do
  valid <- doesDirectoryExist filepath
  if not valid
    then pure $ CommandResult "" (Just $ "Invalid filepath: " ++ filepath) Nothing
    else do

      -- Setup process
      let rawCmd = command f
          processSpec = (shell rawCmd)
            { cwd = Just filepath
            , std_out = CreatePipe
            , std_err = CreatePipe
            }

      -- Start the process
      (_, Just hout, Just herr, phandle) <- createProcess processSpec

      -- Set buffering explicitly to line buffering (not NoBuffering)
      hSetBuffering hout LineBuffering
      hSetBuffering herr LineBuffering

      -- Read and fully evaluate outputs before waiting for the process
      rawOut <- hGetContents hout
      rawErr <- hGetContents herr
      stdout_txt <- evaluate (force rawOut)
      stderr_txt <- evaluate (force rawErr)

      -- Now wait for the process to finish
      exitCode <- waitForProcess phandle

      -- Handle results
      case exitCode of
        ExitFailure code -> do
          let errMsg = "Process exited with code " ++ show code ++ " from path: " ++ filepath ++ ":\n" ++ onFail f rawCmd stderr_txt
          when (shouldLog f) $ do
            emit notifier errMsg
          pure $ CommandResult stdout_txt (Just errMsg) (if null stderr_txt then Nothing else Just stderr_txt)

        ExitSuccess ->
          if null stderr_txt
            then do
              when (shouldLog f) $
                emit notifier (onSuccess f rawCmd stdout_txt)
              pure $ CommandResult stdout_txt Nothing Nothing
            else do
              when (shouldLog f) $ do
                let stdErrMsg = "stderr:\n" ++ onStdFail f rawCmd stdout_txt stderr_txt
                emit notifier stdErrMsg
              pure $ CommandResult stdout_txt Nothing (Just stderr_txt)


-- | Filesystem helpers
-- Deletes directory if it exists (raises errors when it fails)
deleteDirectoryIfExists :: FilePath -> IO () -> IO ()
deleteDirectoryIfExists dir f = do
  exists <- doesDirectoryExist dir
  when exists $ do
    _unused <- f
    let cmd = "rmdir /S /Q \"" ++ dir ++ "\""
    callCommand cmd