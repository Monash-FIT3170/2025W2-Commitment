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
    -- Update refcount
    isCreator <- modifyMVar directoryRefCount $ \refMap ->
        case Map.lookup path refMap of
            Nothing -> pure (Map.insert path 1 refMap, True)        -- add entry to map
            Just n  -> pure (Map.insert path (n + 1) refMap, False) -- increment entry

    -- Run actual IO task only if this call is the creator and 
    -- the path does not already exist
    if isCreator
       then withDirectoryLock path $ do
            -- delete it if it already exists, it should not exist yet
            exists <- doesDirectoryExist path
            if exists then deleteDirectoryIfExists path (pure ()) else pure False
            -- create fresh directory
            createDirectoryIfMissing True path
            -- do task to initialise directory
            -- Run the task; if it fails, ensure state and filesystem are cleaned
            (Just <$> task path)
              `catch` \(e :: SomeException) -> do
                  -- emit or log the error if needed
                  -- safePrint $ "[createDirectory] Task failed for " ++ path ++ ": " ++ displayException e
                  -- rollback directory state (both filesystem + refcount + semaphore)
                  deleteDirectory path (pure ())
                  -- rethrow the exception so caller knows it failed
                  throwIO e
                  
       else pure Nothing

-- | Delete a directory safely (only one thread per dir at a time)
deleteDirectory :: FilePath -> IO () -> IO Bool
deleteDirectory path callback = do
    mDelete <- modifyMVar directoryRefCount $ \refMap ->
        case Map.lookup path refMap of
            Nothing -> pure (refMap, False)                       -- nothing to do
            Just 1  -> pure (Map.delete path refMap, True)        -- remove entry 
            Just n  -> if n > 0
                then pure (Map.insert path (n - 1) refMap, False) -- decrement entry if valid
                else pure (refMap, False)                         -- do nothing if n <= 0

    -- Only delete if this call actually drops refcount to zero
    if mDelete
        then withDirectoryLock path $ do
            deleteDirectoryIfExists path callback
            -- 🧹 Clean up semaphore when the directory is fully released
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
fetchDataFrom rawUrl notifier = do
        let url = cleanGitUrl rawUrl
        
        workingDir <- getTemporaryDirectory
        let cloneRoot = workingDir </> "cloned-repos"

        awaitOutsideCloneDir <- createDirectoryIfMissing True cloneRoot
        emit notifier "Validating repo exists..."

        let execCmdInWorkingDir = execAndParse notifier workingDir
        _ <- execCmdInWorkingDir (checkIfRepoExists url) parseRepoExists "Repo does not exist"

        emit notifier "Found the repo!"

        let parts = splitOn '/' url
            repoRelativePath = last (init parts) ++ "/" ++ last parts
            repoAbsPath      = cloneRoot </> repoRelativePath

            cloneFunction = createDirectory repoAbsPath (\_ -> do
                    -- clone the repo in this case, otherwise we don't actually need to clone it as
                    -- it already exists and another request is using it rn
                    emit notifier "Cloning repo..."

                    -- creating the directory to clone into
                    createDirectoryIfMissing True repoAbsPath

                    -- clones git repo into directory 
                    commandResult <- executeCommandTimedOut 10 notifier cloneRoot (cloneRepo url repoAbsPath)
                    let parsedCloneResult = parsed "Failed to clone the repo" $ successful commandResult

                    -- we want to ensure that 
                    let gitDir = repoAbsPath </> ".git"
                    exists <- doesDirectoryExist gitDir
                    unless exists $ do
                        emit notifier "Clone failed, cleaning up..."
                        deleteDirectoryIfExists repoAbsPath (pure ())
                        throwIO (userError "Incomplete clone directory")
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
