{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Types (
  RepositoryData(..),
  BranchData(..),
  CommitData(..),
  ContributorData(..),
  FileChanges(..),
  FileContents(..),
  ChangeType(..),
  getChangeType,
  ChangeData(..),
  ExtraData(..),
  ModifyData(..),
  RenameData(..),
  CopyData(..),
  sortCommitsByTimestamp
) where

import Data.Map.Strict (Map)
import Data.Time (UTCTime)
import GHC.Generics 
import Data.Aeson 
import Data.Aeson.Types (defaultOptions, SumEncoding(..))

-- | Represents the full repository data
data RepositoryData = RepositoryData
  { repoName     :: String
  , branches     :: [BranchData]
  , allCommits   :: Map String CommitData
  , contributors :: Map String ContributorData
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data BranchData = BranchData
  { branchName   :: String
  , commitHashes :: [String]
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data CommitData = CommitData
  { commitHash      :: String
  , commitTitle     :: String
  , contributorName :: String
  , description     :: String
  , timestamp       :: UTCTime
  , fileData        :: [FileChanges]
  , diff            :: String
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data ContributorData = ContributorData
  { name :: String
  , emails           :: [String]
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data FileChanges = FileChanges
  { filepath     :: String
  , oldFilePath  :: String
  , char         :: ChangeType
  , likeness     :: Int
  , newLines     :: Int
  , deletedLines :: Int
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Change types: A = Added, M = Modified, D = Deleted, R = Renamed, C = Copied
data ChangeType = A | M | D | R | C
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, Read)

getChangeType :: Char -> Maybe ChangeType
getChangeType 'A' = Just A
getChangeType 'M' = Just M
getChangeType 'D' = Just D
getChangeType 'R' = Just R
getChangeType 'C' = Just C
getChangeType  _  = Nothing

-- | Sorting function (newest first)
sortCommitsByTimestamp :: CommitData -> CommitData -> Ordering
sortCommitsByTimestamp c1 c2 = compare (timestamp c2) (timestamp c1)