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
import Control.Monad (when)
import Data.IORef

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

type DirectoryMap = IORef (Map.Map FilePath Int)

{-# NOINLINE directoryMap #-}
directoryMap :: MVar (Map FilePath Int)
directoryMap = unsafePerformIO (newMVar Map.empty)

createDirectory :: FilePath -> (FilePath -> IO ()) -> IO ()
createDirectory task path = modifyMVar_ directoryMap $ \dirMap ->
    case Map.lookup path dirMap of
        Nothing -> do
            createDirectoryIfMissing True path
            task path
            pure (Map.insert path 1 dirMap)
        Just n ->
            pure (Map.insert path (n + 1) dirMap)

deleteDirectory :: FilePath -> (FilePath -> IO ()) -> IO () -> IO ()
deleteDirectory task path callback = modifyMVar_ directoryMap $ \dirMap ->
    case Map.lookup path dirMap of
        Nothing -> pure dirMap
        Just 1 -> do
            deleteDirectoryIfExists path callback
            task path
            pure (Map.delete path dirMap)
        Just n ->
            pure (Map.insert path (n - 1) dirMap)

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

        let parts = splitOn '/' url
            repoNameFromUrl = last (init parts) ++ "/" ++ last parts
            repoRelativePath = "cloned-repos" </> repoNameFromUrl
            repoAbsPath = workingDir </> repoRelativePath

        awaitCloneResult <- parsed "Failed to clone the repo" <$> await (submitTaskAsync commandPool
            (\path -> do
                -- create the sub-directory using createDirectory function to manage 
                -- IO resources
                awaitCreateRepoDir <- createDirectory 
                (\_path -> do
                    -- clone the repo in this case, otherwise we don't actually need to clone it as
                    -- it already exists and another request is using it rn
                    emit notifier "Cloning repo..."
                    -- creating the source directory to clone into
                    let source_dir = path </> "source"
                    awaitSource <- createDirectoryIfMissing True source_dir
                    -- clones git repo into source directory 
                    awaitClone <- successful <$> executeCommandTimedOut 5 notifier workingDir (cloneRepo url source_dir)
                    -- copy contents of source directory to other sub-directories so that worker threads can access them efficiently]
                    copyAndDistrobute path source_dir $ __nWorkers commandPool
                    ) repoAbsPath
            ) repoAbsPath)

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
        let parts = splitOn '/' url
            repoNameFromUrl = last (init parts) ++ "/" ++ last parts
            repoRelativePath = "cloned-repos" </> repoNameFromUrl
            repoAbsPath = workingDir </> repoRelativePath

        deleteDirectory (\_ -> pure ()) repoAbsPath (emit notifier "Cleaning Up Directory...")


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
    allCommitData <- passAllAsync commandPool parsingPool
        (\(c1, c2) -> do
            let doCommitCommand = executeCommand notifier path
            r1 <- doCommitCommand c1
            r2 <- doCommitCommand c2
            pure (r1, r2)
        )
        (\(r1, r2) -> do
            let msg              = "Failed to formulate all commit data"
                checkPass        = parsed msg . successful
                raw1             = checkPass r1
                raw2             = checkPass r2
                -- parsing information
                ibCommitData     = parsed msg $ parseCommitData raw1
                metaFileInfoList = map (parsed msg . parseFileDataFromCommit) $ involvedFiles ibCommitData
                metaFileDiffChanges = parsed msg $ parseFileDataFromDiff raw2
                -- link file data together
                fileData = mergeFileMetaData $ pairByFilePath metaFileInfoList metaFileDiffChanges

            count <- atomically $ do
                modifyTVar' commitCounter (+1)
                readTVar commitCounter
            emit notifier $ "Formulating all commit data (" ++ show count ++ "/" ++ show commitsFound ++ ")..."

            pure $ CommitData
                (ibCommitHash      ibCommitData ) --commitHash        
                (ibCommitTitle     ibCommitData ) --commitTitle     
                (ibContributorName ibCommitData ) --contributorName 
                (ibDescription     ibCommitData ) --description     
                (ibTimestamp       ibCommitData ) --timestamp       
                (fileData                       ) --fileData
            )
        (map (\h -> (getCommitDetails h, getCommitDiff h)) allCommitHashes)

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
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn delim = go []
  where
    go acc [] = [reverse acc]
    go acc (x:xs)
      | x == delim = reverse acc : go [] xs
      | otherwise  = go (x:acc) xs

lastSplit :: Eq a => a -> [a] -> [a]
lastSplit delim = reverse . takeWhile (/= delim) . reverse

unique :: Ord a => [a] -> [a]
unique = Map.keys . Map.fromList . flip zip (repeat ())

replace :: String -> String -> String -> String
replace old new str
    | old `isPrefixOf` str = new ++ drop (length old) str
    | null str             = ""
    | otherwise            = head str : replace old new (tail str)
