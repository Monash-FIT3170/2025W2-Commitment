{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use join" #-}
{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Use tuple-section" #-}

module Commitment (
    fetchDataFrom
) where

import Control.Concurrent.STM
import Control.Exception (catch, SomeException (SomeException), displayException, finally)
import Data.List (sortBy, isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Map.Strict ()
import qualified Data.Map.Strict as Map
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)
import System.Directory (getCurrentDirectory, createDirectoryIfMissing)
import Control.Concurrent (getNumCapabilities)

import Types
import Threading
import Command
import GitCommands
import Parsing

-- Create global thread pools
{-# NOINLINE parsingPool #-}
parsingPool :: WorkerPool
parsingPool = unsafePerformIO $ getNumCapabilities >>= \n -> createThreadPool n "Parsing Pool"

{-# NOINLINE commandPool #-}
commandPool :: WorkerPool
commandPool = unsafePerformIO $ getNumCapabilities >>= \n -> createThreadPool (n * 8) "Command Pool"

zipWithFunctions :: [[[a]]] -> [[a] -> b] -> [[([a], [a] -> b)]]
zipWithFunctions = zipWith (\inner f -> map (\x -> (x, f)) inner)

-- automatically execute shell scripts in the command pool whilst the command result is extracted and passed to the parsing pool
execAndParse :: TBQueue String -> FilePath -> Command -> (String -> ParseResult a) -> String -> IO a
execAndParse notifier cwd cmd parser msg = await (
        passThroughAsync commandPool parsingPool
        (executeCommand notifier cwd)
        (pure . parsed msg . parser . parsed msg . successful)
        cmd
    )

execAndParseAll :: TBQueue String -> FilePath -> [Command] -> (String -> ParseResult a) -> String -> IO [a]
execAndParseAll notifier cwd cmds parser msg = passAllAsync commandPool parsingPool
    (executeCommand notifier cwd)
    (pure . parsed msg . parser . parsed msg . successful)
    cmds

fetchDataFrom :: String -> TBQueue String -> IO (Either String RepositoryData)
fetchDataFrom url notifier = (do
        workingDir <- getCurrentDirectory
        let cloneRoot = workingDir </> "cloned-repos"

        awaitOutsideCloneDir <- createDirectoryIfMissing True cloneRoot
        emit notifier "Validating repo exists..."

        let execCmdInWorkingDir = execAndParse notifier workingDir
        _ <- execCmdInWorkingDir (checkIfRepoExists url) parseRepoExists "Repo does not exist"

        emit notifier "Found the repo!"

        let repoNameFromUrl = lastSplit '/' url
            repoRelativePath = "cloned-repos" </> repoNameFromUrl
            repoAbsPath = workingDir </> repoRelativePath

        awaitRepoDirDeletion <- deleteDirectoryIfExists repoAbsPath (emit notifier "Cleaning Up Directory...")
        awaitCreateRepoDir   <- createDirectoryIfMissing True repoAbsPath

        emit notifier "Cloning repo..."
        awaitCloneResult <- parsed "Failed to clone the repo" <$> await (submitTaskAsync commandPool
            (\path -> successful <$> executeCommandTimedOut 10 notifier workingDir (cloneRepo url path))
            repoAbsPath) 

        emit notifier "Getting repository data..."
        repoData <- formulateRepoData url repoAbsPath notifier
        emit notifier "Data processed!"
        pure (Right repoData)
    )
    `catch` \(e :: SomeException) -> do
        let errMsg = displayException e
        emit notifier ("Error occurred:\n" ++ errMsg)
        pure (Left errMsg)
    `finally` do
        -- cleanup no matter what
        workingDir <- getCurrentDirectory
        let repoNameFromUrl = lastSplit '/' url
            repoAbsPath     = workingDir </> "cloned-repos" </> repoNameFromUrl
        deleteDirectoryIfExists repoAbsPath (emit notifier "Cleaning Up Directory...")
    

-- | High-level function to orchestrate parsing, transforming, and assembling data
formulateRepoData :: String -> FilePath -> TBQueue String -> IO RepositoryData
formulateRepoData _url path notifier = do
    let execCmd = execAndParse notifier path
        execAll = execAndParseAll notifier path

    emit notifier "Searching for branch names..."
    collectedBranchNames <- execCmd getBranches parseRepoBranches "Failed to parse git branch names"
    let branchNameFilter = replace "remotes/" "" . replace "origin/" ""
        branchNames = unique collectedBranchNames
        filteredBranchNames = map branchNameFilter branchNames 

    emit notifier "Searching for commit hashes..."
    allCommitHashesListOfList <- execAll (map getAllCommitsFrom branchNames) parseCommitHashes "Failed to parse commit hashes from branches"

    let allCommitHashes = unique $ concat allCommitHashesListOfList
        commitsFound = length allCommitHashes

    commitCounter <- newTVarIO 0
    emit notifier "Formulating all commit data..."

    commitMetaData <- passAllAsync commandPool parsingPool
        (executeCommand notifier path)
        (\cr -> do
            let msg = "Failed to formulate all commit data" 
                res = parsed msg . parseCommitData . parsed msg . successful $ cr
            count <- atomically $ do
                modifyTVar' commitCounter (+1)
                readTVar commitCounter
            emit notifier $ "Formulating all commit data (" ++ show count ++ "/" ++ show commitsFound ++ ")..."
            pure res
            )
        (map getCommitDetails allCommitHashes)    

    let filesFound = sum (map (length . involvedFiles) commitMetaData)
    fileCounter <- newTVarIO 0
    emit notifier $ "Found " ++ show filesFound ++ " distinct file versions across all commits"

    let commitMetaDataParsingPrimer = map (\commitData -> do -- [([String] -> IO (Maybe(String, String, [String])))]
            let hash = ibCommitHash commitData
                getNew = getFileContents notifier path hash getFileDataFromCommit
                getOld = getFileContents notifier path hash getOldFileDataFromCommit
            extractCommitData getNew getOld  -- [String] -> IO (Maybe(String, String, [String]))
            ) commitMetaData

    allFileData <- parsedNestedLists "Failed to formulate all file data" <$> passNestedAsync commandPool parsingPool
        (\(filedata, extractFrom) -> extractFrom filedata)
        (\case
            Error msg -> pure $ Error msg
            Result (n, o, fd) -> do
                let res = parseFileDataFromCommit n o fd
                count <- atomically $ do
                    modifyTVar' fileCounter (+1)    
                    readTVar fileCounter
                emit notifier $ "Formulating all file data (" ++ show count ++ "/" ++ show filesFound ++ ")..."
                pure res
        )
        (zipWithFunctions (map involvedFiles commitMetaData) commitMetaDataParsingPrimer)

    emit notifier "Joining found files with commit data..."
    let allCommitData = zipWith (\meta files -> CommitData
            (ibCommitHash meta)
            (ibCommitTitle meta)
            (ibContributorName meta)
            (ibDescription meta)
            (ibTimestamp meta)
            files) commitMetaData allFileData

    emit notifier "Formulating all contributors..."
    let uniqueNames = unique $ map contributorName allCommitData
    nameToEmails <- execAll (map getContributorEmails uniqueNames) parseContributorEmails "Failed to formulate contributor emails"

    let allContributors = zipWith ContributorData uniqueNames nameToEmails
        contributorMap = Map.fromList [(name c, c) | c <- allContributors]
        commitMap = Map.fromList [(commitHash c, c) | c <- allCommitData]

    emit notifier "Linking branches to their commits..."
    let branchToCommitsMap = Map.fromList $ zip filteredBranchNames allCommitHashesListOfList

    emit notifier "Formulating all branch data..."
    let sortByTime h1 h2 = sortCommitsByTimestamp (commitMap Map.! h1) (commitMap Map.! h2)

    let branchData = map (\branch -> do
            let hashes = fromMaybe [] $ Map.lookup branch branchToCommitsMap
            let sortedHashes = sortBy sortByTime hashes
            BranchData branch sortedHashes
            ) filteredBranchNames

    emit notifier "Fetching repo name..."
    repoFetchedName <- execCmd getRepoName parseRepoName "Failed to retrieve repo name"

    pure $ RepositoryData
        repoFetchedName
        branchData
        commitMap
        contributorMap

-- | Utility
lastSplit :: Eq a => a -> [a] -> [a]
lastSplit delim = reverse . takeWhile (/= delim) . reverse

unique :: Ord a => [a] -> [a]
unique = Map.keys . Map.fromList . flip zip (repeat ())

replace :: String -> String -> String -> String
replace old new str
    | old `isPrefixOf` str = new ++ drop (length old) str
    | null str             = ""
    | otherwise            = head str : replace old new (tail str)