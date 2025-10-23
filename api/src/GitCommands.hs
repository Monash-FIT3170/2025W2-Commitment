--{-# LANGUAGE OverloadedStrings #-}

module GitCommands (
  quote, 
  delim,
  checkIfRepoExists,
  cloneRepo,
  checkIsGitDirectory,
  getBranches,
  getAllCommitsFrom,
  getContributorEmails,
  getCommitDetails,
  getCommitDiff,
  getRepoName
) where

import Command
import Control.Concurrent.STM ( TBQueue )

quote :: String -> String
quote s = "\"" ++ s ++ "\""

delim :: String
delim  = "\\n|||END|||"

checkIfRepoExists :: String -> Command
checkIfRepoExists url = doNotLogData
  { command = "git ls-remote " ++ quote url
  , onSuccess = \_ _ -> "Found Repo " ++ url
  , onFail = \_ e -> "Could not find Repo (" ++ url ++ "): " ++ e
  }

cloneRepo :: String -> FilePath -> Command
cloneRepo url targetDirectory = doNotLogData
  { command =
      "git -c credential.helper= -c core.askPass=true clone --bare "
      ++ quote url ++ " " ++ quote targetDirectory
  , env_vars = Just [("GIT_TERMINAL_PROMPT", "0")]
  , env_clean = ["GIT_ASKPASS","GCM_INTERACTIVE"]
  , onSuccess = \_ _ -> url ++ " successfully cloned to " ++ targetDirectory
  , onFail = \c e -> "Error cloning repo:\nCommand:\n" ++ c ++ "\nError Message:\n" ++ e
  }

checkIsGitDirectory :: FilePath -> Command
checkIsGitDirectory dir = doNotLogData
  { command = "git -C " ++ quote dir ++ " rev-parse --git-dir"
  }

getBranches :: Command
getBranches = doNotLogData
  { command = "git --no-pager branch -a --format=" ++ quote "%(refname:short)"
  }

getAllCommitsFrom :: String -> Command
getAllCommitsFrom branch = doNotLogData
  { command = "git --no-pager log " ++ quote branch ++ " --format=%H"
  }

getContributorEmails :: String -> Command
getContributorEmails name = doNotLogData
  { command = "git log \"--author=" ++ name ++ "\" \"--pretty=format:%ae\""
  }

getCommitDetails :: String -> Command
getCommitDetails hash = doNotLogData
  { command = "git show \"--pretty=format:%H" ++ delim ++ "%an" ++ delim ++ "%cI" ++ delim ++ "%s" ++ delim ++ "%b" ++ delim ++ "\" \"--name-status\" " ++ hash 
  }

getCommitDiff :: String -> Command
getCommitDiff hash = doNotLogData
  { command = "git --no-pager diff-tree -p " ++ hash 
  }

getRepoName :: Command
getRepoName = doNotLogData
  { command = "git remote get-url origin"
  }
