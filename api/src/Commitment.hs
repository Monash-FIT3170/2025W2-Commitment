{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use join" #-}
{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Use tuple-section" #-}

module Commitment (
    fetchDataFrom,
    parsingPool,
    commandPool
) where

import Control.Concurrent.STM
import Control.Exception (catch, SomeException (SomeException), displayException, bracket)
import Data.List (sortBy, isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Map.Strict ()
import qualified Data.Map.Strict as Map
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import System.Directory (getCurrentDirectory, createDirectoryIfMissing)
import Control.Concurrent (
    MVar,
    newMVar,
    modifyMVar_,
    modifyMVar,
    withMVar,
    readMVar,
    putMVar,
    tryTakeMVar,
    getNumCapabilities,
    newEmptyMVar )
import Control.Monad (when)
import Data.IORef

import Types
import Threading
import Command
import GitCommands
import Parsing

-- Create global thread pools
-- Initialize RTS and return pools
{-# NOINLINE globalPools #-}
globalPools :: (WorkerPool, WorkerPool)
globalPools = unsafePerformIO $ do
    cap <- getNumCapabilities
    -- Create the pools
    parsing <- createThreadPool cap "Parsing Pool"
    command <- createThreadPool (cap * 4) "Command Pool"
    pure (parsing, command)

-- Expose them as pure values
{-# NOINLINE parsingPool #-}
parsingPool :: WorkerPool
parsingPool = fst globalPools

{-# NOINLINE commandPool #-}
commandPool :: WorkerPool
commandPool = snd globalPools

-- global Disk resource registeries
type RefCountMap = Map.Map FilePath Int
type SemaphoreMap = Map.Map FilePath (MVar ())

-- Global registry of per-directory locks
{-# NOINLINE directoryRefCount #-}
directoryRefCount :: MVar RefCountMap
directoryRefCount = unsafePerformIO (newMVar Map.empty)

-- Global registry of per-directory semaphores to store whether it exists or not
{-# NOINLINE directorySemaphores #-}
directorySemaphores :: MVar SemaphoreMap
directorySemaphores = unsafePerformIO (newMVar Map.empty)

-- | Get the semaphore for a directory, creating one if it doesn't exist.
getDirSemaphore :: FilePath -> IO (MVar ())
getDirSemaphore path = modifyMVar directorySemaphores $ \semMap ->
    case Map.lookup path semMap of
        Just sem -> pure (semMap, sem)
        Nothing -> do
            sem <- newMVar ()
            pure (Map.insert path sem semMap, sem)

-- | Acquire the per-directory lock and run an action.
withDirectoryLock :: FilePath -> IO a -> IO a
withDirectoryLock path action = do
    sem <- getDirSemaphore path
    withMVar sem $ const action

-- | Create a directory if needed and run a task, tracking refcount.
createDirectory :: FilePath -> (FilePath -> IO a) -> IO (Maybe a)
createDirectory path task = do
    -- Update refcount
    isCreator <- modifyMVar directoryRefCount $ \refMap ->
        case Map.lookup path refMap of
            Nothing -> pure (Map.insert path 1 refMap, True)
            Just n  -> pure (Map.insert path (n + 1) refMap, False)

    -- Run actual IO task only if this call is the creator
    if isCreator
       then withDirectoryLock path $ do
           createDirectoryIfMissing True path
           a <- task path
           pure (Just a)
       else pure Nothing

-- | Delete a directory safely (only one thread per dir at a time)
deleteDirectory :: FilePath -> IO () -> IO ()
deleteDirectory path callback = do
    mDelete <- modifyMVar directoryRefCount $ \refMap ->
        case Map.lookup path refMap of
            Nothing -> pure (refMap, False)                       -- nothing to do
            Just n  -> if n < 2
                then pure (Map.delete path refMap, True)          -- remove entry
                else pure (Map.insert path (n - 1) refMap, False) -- decrement entry

    -- Only delete if this call actually drops refcount to zero
    when mDelete $ withDirectoryLock path $ deleteDirectoryIfExists path callback

zipWithFunctions :: [[[a]]] -> [[a] -> b] -> [[([a], [a] -> b)]]
zipWithFunctions = zipWith (\inner f -> map (\x -> (x, f)) inner)

-- automatically execute shell scripts in the command pool whilst the command result is extracted and passed to the parsing pool
execAndParseNoIndex :: TBQueue String -> FilePath -> Command -> (String -> ParseResult a) -> String -> IO a
execAndParseNoIndex notifier cwd cmd parser msg = await (
        passThroughAsync commandPool parsingPool
        (executeCommand notifier cwd)
        (pure . parsed msg . parser . parsed msg . successful)
        cmd
    )

execAndParse :: TBQueue String -> FilePath -> Command -> (String -> ParseResult a) -> String -> IO a
execAndParse notifier cwd cmd parser msg = await (
        passThroughAsyncIndexed commandPool parsingPool
        (\command idx -> do
            let workingDir = cwd </> show idx
            executeCommand notifier workingDir command)
        (pure . parsed msg . parser . parsed msg . successful)
        cmd
    )

execAndParseAll :: TBQueue String -> FilePath -> [Command] -> (String -> ParseResult a) -> String -> IO [a]
execAndParseAll notifier cwd cmds parser msg = passAllAsyncIndexed commandPool parsingPool
    (\command idx -> do
        let workingDir = cwd </> show idx
        executeCommand notifier workingDir command)
    (pure . parsed msg . parser . parsed msg . successful)
    cmds

fetchDataFrom :: String -> TBQueue String -> IO (Either String RepositoryData)
fetchDataFrom url notifier = do
        workingDir <- getCurrentDirectory
        let cloneRoot = workingDir </> "cloned-repos"

        awaitOutsideCloneDir <- createDirectoryIfMissing True cloneRoot
        emit notifier "Validating repo exists..."

        let execCmdInWorkingDir = execAndParseNoIndex notifier workingDir
        _ <- execCmdInWorkingDir (checkIfRepoExists url) parseRepoExists "Repo does not exist"

        emit notifier "Found the repo!"

        let parts = splitOn '/' url
            repoNameFromUrl  = last (init parts) ++ "/" ++ last parts
            repoRelativePath = "cloned-repos" </> repoNameFromUrl
            repoAbsPath      = workingDir </> repoRelativePath
            sourceDir        = repoAbsPath </> "source"

            cloneFunction = createDirectory repoAbsPath (\_ -> do
                    -- clone the repo in this case, otherwise we don't actually need to clone it as
                    -- it already exists and another request is using it rn
                    emit notifier "Cloning repo..."

                    -- creating the source directory to clone into
                    createDirectoryIfMissing True sourceDir

                    -- clones git repo into source directory 
                    cloneResult <- successful <$> executeCommandTimedOut 5 notifier workingDir (cloneRepo url sourceDir)

                    -- assert the error in this function so that the next steps don't happen if its faulty
                    let parsedCloneResult = parsed "Failed to clone the repo" cloneResult

                    -- submit tasks to the commandPool in parallel to copy to other directories if 
                    -- copy contents of source directory to other sub-directories so that worker threads can access them efficiently
                    submitAllAsync commandPool (\idx -> do
                        let targetDir = repoAbsPath </> show idx
                        copyDirectory sourceDir targetDir
                        ) [0 .. numWorkers commandPool - 1]

                    pure ()
                )

            deleteFunction _ = deleteDirectory repoAbsPath (emit notifier "Cleaning Up Directory...")

        bracket
            -- clone the repository if it has not already been
            cloneFunction
            -- delete the repository if it can be
            deleteFunction
            -- run this part inside to purify any side effects
            (\_ -> do
                emit notifier "Getting repository data..."
                repoData <- formulateRepoData url repoAbsPath notifier
                emit notifier "Data processed!"
                pure (Right repoData)
            )
            `catch` \(e :: SomeException) -> do
            let errMsg = displayException e
            emit notifier ("Error occurred:\n" ++ errMsg)
            pure (Left errMsg)


-- | High-level function to orchestrate parsing, transforming, and assembling data
formulateRepoData :: String -> FilePath -> TBQueue String -> IO RepositoryData
formulateRepoData _url path notifier = do
    let source = path </> "source"
        execCmd = execAndParse notifier path
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
    allCommitData <- passAllAsyncIndexed commandPool parsingPool
        (\(c1, c2) idx -> do
            let workingDir = path </> show idx
                doCommitCommand = executeCommand notifier workingDir
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
