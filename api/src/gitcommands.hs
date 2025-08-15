--{-# LANGUAGE OverloadedStrings #-}

module GitCommands (
  checkIfRepoExists,
  cloneRepo,
  getBranches,
  getAllCommitsFrom,
  getContributorEmails,
  getCommitDetails,
  getFileContents,
  getFileDataFromCommit,
  getOldFileDataFromCommit,
  getRepoName
) where

import Command
import Control.Concurrent.STM ( TBQueue )

quote :: String -> String
quote s = "\"" ++ s ++ "\""

checkIfRepoExists :: String -> Command
checkIfRepoExists url = doNotLogData
  { command = "git ls-remote " ++ quote url
  , onSuccess = \_ _ -> "Found Repo " ++ url
  , onFail = \_ e -> "Could not find Repo (" ++ url ++ "): " ++ e
  }

cloneRepo :: String -> FilePath -> Command
cloneRepo url targetDirectory = doNotLogData
  { command = "git clone --no-checkout " ++ quote url ++ " " ++ quote targetDirectory
  , onSuccess = \_ _ -> url ++ " successfully cloned to " ++ targetDirectory
  , onFail = \c e -> "Error cloning repo:\nCommand:\n" ++ c ++ "\nError Message:\n" ++ e
  }

getBranches :: Command
getBranches = doNotLogData
  { command = "git branch"
  }

getAllCommitsFrom :: String -> Command
getAllCommitsFrom branch = doNotLogData
  { command = "git log " ++ branch ++ " --format=%H"
  }

getContributorEmails :: String -> Command
getContributorEmails name = doNotLogData
  { command = "git log \"--author=" ++ name ++ "\" \"--pretty=format:%ae\""
  }

getCommitDetails :: String -> Command
getCommitDetails hash = doNotLogData
  { command = "git show \"--pretty=format:%H\\n|||END|||%an\\n|||END|||%ad\\n|||END|||%s\\n|||END|||%b\\n|||END|||\" \"--diff-filter=ADMRC\" \"--name-status\" " ++ hash 
  }

getFileContents :: TBQueue String -> FilePath -> String -> (String -> String -> Command) -> String -> IO String
getFileContents notifier path hash cmd_f file =
  getParsableStringFromCmd <$> executeCommand notifier path (cmd_f hash file)

getFileDataFromCommit :: String -> FilePath -> Command
getFileDataFromCommit hash path = doNotLogData
  { command = "git show " ++ hash ++ ":" ++ path
  }

getOldFileDataFromCommit :: String -> FilePath -> Command
getOldFileDataFromCommit hash path = doNotLogData
  { command = "git show " ++ hash ++ "~1:" ++ path
  }

getRepoName :: Command
getRepoName = doNotLogData
  { command = "git remote get-url origin"
  }
