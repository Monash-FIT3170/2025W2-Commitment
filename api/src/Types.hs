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
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data ContributorData = ContributorData
  { name :: String
  , emails           :: [String]
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data FileChanges = FileChanges
  { file    :: FileContents
  , changes :: ChangeData
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data FileContents = FileContents
  { filepath :: String
  , contents :: String
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

data ChangeData = ChangeData
  { char  :: ChangeType
  , extra :: Maybe ExtraData
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data ExtraData
  = Modify ModifyData
  | Rename RenameData
  | Copy CopyData
  deriving (Show, Eq, Generic)

instance ToJSON ExtraData where
  toJSON = genericToJSON defaultOptions { sumEncoding = UntaggedValue }

instance FromJSON ExtraData where
  parseJSON = genericParseJSON defaultOptions { sumEncoding = UntaggedValue }

data ModifyData = ModifyData
  { previousFile :: FileContents
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data RenameData = RenameData
  { oldFilePath :: String
  , likeness    :: Int
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data CopyData = CopyData
  { copyOldFilePath :: String
  , copyLikeness    :: Int
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Monoid instance for FileContents (concatenates contents and uses longest path)
instance Semigroup FileContents where
  FileContents c1 p1 <> FileContents c2 p2 =
    FileContents (c1 <> c2) (if length p1 >= length p2 then p1 else p2)

instance Monoid FileContents where
  mempty = FileContents "" ""

-- | Sorting function (newest first)
sortCommitsByTimestamp :: CommitData -> CommitData -> Ordering
sortCommitsByTimestamp c1 c2 = compare (timestamp c2) (timestamp c1)