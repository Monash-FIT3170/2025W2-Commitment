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
import Control.Exception (
    catch,
    finally,
    SomeException (SomeException),
    displayException,
    bracket,
    throwIO)
import Data.List (sortBy, isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Map.Strict ()
import qualified Data.Map.Strict as Map
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import System.Directory (
    getTemporaryDirectory,
    createDirectoryIfMissing,
    doesDirectoryExist)
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
import Control.Monad (when, void, unless)
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
    -- Acquire the per-directory lock to prevent concurrent initialization
    withDirectoryLock path $ do
        mEntry <- modifyMVar directoryRefCount $ \refMap ->
            case Map.lookup path refMap of
                Nothing -> pure (refMap, True)                         -- directory not yet created
                Just n  -> pure (Map.insert path (n + 1) refMap, False) -- increment counter

        if mEntry then do
            -- creating the directory to initialise
            createDirectoryIfMissing True path
            -- Directory is not initialized, run the creation task
            -- which initialises the directory with all the correct information
            result <- task path
            -- Only after successful task, insert entry into map with counter = 1
            modifyMVar_ directoryRefCount $ \refMap ->
                pure (Map.insert path 1 refMap)
            pure (Just result)
        else pure Nothing

-- | Delete a directory safely (only one thread per dir at a time)
deleteDirectory :: FilePath -> IO () -> IO Bool
deleteDirectory path callback =
    -- Acquire the directory lock first to avoid circular locking
    withDirectoryLock path $ do
        -- Now safely modify the refcount
        mDelete <- modifyMVar directoryRefCount $ \refMap ->
            case Map.lookup path refMap of
                Nothing -> pure (refMap, False)                       -- nothing to do
                Just 1  -> pure (Map.delete path refMap, True)        -- remove entry 
                Just n  -> if n > 0
                    then pure (Map.insert path (n - 1) refMap, False) -- decrement entry
                    else pure (refMap, False)                         -- do nothing if n <= 0

        -- Only delete if this call actually drops refcount to zero
        if mDelete
            then do
                deleteDirectoryIfExists path callback
                -- 🧹 Clean up semaphore after deletion
                modifyMVar_ directorySemaphores $ pure . Map.delete path
                pure True
            else pure False

-- automatically execute shell scripts in the command pool whilst the command result is extracted and passed to the parsing pool
execAndParse :: TBQueue String -> FilePath -> Command -> (String -> ParseResult a) -> String -> IO a
execAndParse notifier cwd cmd parser msg = await (
        passThroughAsync commandPool parsingPool
        (executeCommand notifier cwd)
        (pure . parsed msg . parser . parsed msg . successful)
        cmd
    )

execAndParseAll :: TBQueue String -> FilePath -> [Command] -> (String -> ParseResult a) -> String -> IO [a]
execAndParseAll notifier cwd cmds parser msg =
    passAllAsync commandPool parsingPool
    (executeCommand notifier cwd)
    (pure . parsed msg . parser . parsed msg . successful)
    cmds

fetchDataFrom :: String -> TBQueue String -> IO (Either String RepositoryData)
fetchDataFrom rawUrl notifier = (do
        let url = cleanGitUrl rawUrl

        workingDir <- getTemporaryDirectory
        let cloneRoot = workingDir </> "cloned-repos"

        createDirectoryIfMissing True cloneRoot
        emit notifier "Validating repo exists..."

        let execCmdInWorkingDir = execAndParse notifier workingDir
        _ <- execCmdInWorkingDir (checkIfRepoExists url) parseRepoExists "Repo does not exist"

        emit notifier "Found the repo!"

        let parts = splitOn '/' url
            repoRelativePath = last (init parts) ++ "/" ++ last parts
            repoAbsPath      = cloneRoot </> repoRelativePath

        -- if this fails we catch it but do not delete
        awaitClone <- createDirectory repoAbsPath $ \_ -> do
                emit notifier "Cloning repo..."
                commandResult <- executeCommandTimedOut 10 notifier cloneRoot (cloneRepo url repoAbsPath)
                let _parsedCloneResult = parsed "Failed to clone the repo" $ successful commandResult

                -- Ensure git directory
                ensureSuccess <- executeCommand notifier repoAbsPath (checkIsGitDirectory repoAbsPath)
                let _ensureSuccessResult = parsed "Failed to initialise the filepath" $ successful ensureSuccess
                
                pure ()

        -- this part should be safe to execute 
        -- and always delete because we have
        -- successfully created the repository
        let processing = do 
                emit notifier "Getting repository data..."
                repoData <- formulateRepoData url repoAbsPath notifier
                emit notifier "Data processed!"
                pure (Right repoData)
        
        processing `finally` 
            deleteDirectory repoAbsPath (emit notifier "Cleaning Up Directory...")

    ) `catch` \(e :: SomeException) -> do
        let errMsg = "Encountered error:\n" ++ displayException e
        emit notifier errMsg
        safePrint errMsg
        pure (Left errMsg)


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
