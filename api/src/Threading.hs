{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use join" #-}

module Threading (
  safePrint,
  emit,
  WorkerPool,
  createThreadPool,
  submit,
  await,
  wrapped,
  submitTask,
  submitTaskAsync,
  submitAll,
  submitNested,
  passThrough,
  passAll,
  passNested,
  passThroughAsync,
  passAllAsync,
  passNestedAsync
) where

import Control.Concurrent.STM (
  atomically, 
  newTQueueIO, 
  readTQueue, 
  writeTQueue, 
  TQueue, 
  writeTBQueue, 
  TBQueue )
import Control.Concurrent
    ( forkIO,
      MVar,
      withMVar,
      newMVar,
      forkIO,
      newEmptyMVar,
      putMVar,
      takeMVar )
import Control.Monad (forever, replicateM_, join, void)
import System.IO.Unsafe (unsafePerformIO)
import System.IO (hFlush, stdout)
import Control.DeepSeq (force)
import Control.Exception
    (SomeException, evaluate, handle, try, throwIO) 

-- ThreadPool abstraction
data WorkerPool = WorkerPool
  { __nWorkers  :: Int,
    __name      :: String, 
    __taskQueue :: TQueue (IO ()) }


-- Shared globally
stdoutLock :: MVar ()
stdoutLock = unsafePerformIO $ newMVar ()
{-# NOINLINE stdoutLock #-}

safePrint :: String -> IO ()
safePrint msg = withMVar stdoutLock $ \_ -> do
  putStrLn msg  
  hFlush stdout

-- | Emits messages for frontend/notifier simulation
emit :: TBQueue String -> String -> IO ()
emit q msg = do 
  res <- evaluate (force msg)
  atomically $ writeTBQueue q res

-- Create a thread pool with N worker threads
createThreadPool :: Int -> String -> IO WorkerPool
createThreadPool numWorkers name = do
  queue <- newTQueueIO
  let pool = WorkerPool numWorkers name queue
  replicateM_ numWorkers $ forkIO $ workerLoop queue
  return pool

-- Worker loop that never dies: catches exceptions from tasks and ignores them,
-- then continues looping.
workerLoop :: TQueue (IO ()) -> IO ()
workerLoop q = forever $ do
  task <- atomically $ readTQueue q
  handle (\(_ :: SomeException) -> pure ()) (void task)

await :: IO (IO a) -> IO a
await = join

wrapped :: a -> IO a
wrapped = pure 

-- Submit a task to the pool, returning an IO that produces the result.
-- The returned IO, when executed (takeMVar), will re-throw the exception
-- if the task failed, or return the successful value.
submit :: WorkerPool -> IO a -> IO (IO a)
submit (WorkerPool _ _ queue) action = do
  resultVar <- newEmptyMVar :: IO (MVar (Either SomeException a))
  -- Wrap the action with 'catch' to always put something in resultVar
  let wrappedAction = do
        r <- try (action >>= evaluate)  -- :: IO (Either SomeException a)
        putMVar resultVar r
  atomically $ writeTQueue queue wrappedAction
  -- Return an IO that will either rethrow the exception or return the value
  return $ do
    e <- takeMVar resultVar
    case e of
      Left ex -> throwIO ex
      Right v -> return v

-- Submit a pure function to run in the pool (returns future)
submitTask :: WorkerPool -> (a -> b) -> a -> IO (IO b)
submitTask pool f x = submit pool (return (f x))

-- Submit a pure function to run in the pool 
submitTaskAsync :: WorkerPool -> (a -> IO b) -> a -> IO (IO b)
submitTaskAsync pool f x = submit pool (f x)

-- Submit a collection of inputs to run in parallel (returns futures)
submitAll :: WorkerPool -> (a -> b) -> [a] -> IO [IO b]
submitAll pool f = mapM (submitTask pool f)

submitNested :: WorkerPool -> (a -> b) -> [[a]] -> IO (IO [[b]])
submitNested pool f nested = do
  -- Submit all tasks (flattened), keeping nested structure
  promises <- mapM (submitAll pool f) nested -- [[IO b]]
  -- Turn [[IO b]] into IO [[b]]
  return $ mapM sequence promises

-- pass two functions to be executed through thread pools, 
-- ensuring the result obtained by the computation from the first pool is then
-- used in the second computation in the second pool as soon as it is ready
passThrough :: WorkerPool -> WorkerPool -> (a -> b) -> (b -> c) -> a -> IO (IO c)
passThrough p1 p2 f1 f2 x = do
  firstFuture <- submitTask p1 f1 x    -- IO (IO b)
  innerResult <- firstFuture           -- IO b
  submitTask p2 f2 innerResult         -- IO (IO c)

passAll :: WorkerPool -> WorkerPool -> (a -> b) -> (b -> c) -> [a] -> IO [c]
passAll p1 p2 f1 f2 xs = do
  mapM (passThrough p1 p2 f1 f2) xs >>= sequence

passNested :: WorkerPool -> WorkerPool -> (a -> b) -> (b -> c) -> [[a]] -> IO [[c]]
passNested p1 p2 f1 f2 =
  mapM (passAll p1 p2 f1 f2)

passThroughAsync :: WorkerPool -> WorkerPool -> (a -> IO b) -> (b -> IO c) -> a -> IO (IO c)
passThroughAsync p1 p2 f1 f2 x = do
  futureB <- submit p1 (f1 x)            -- IO (IO b)
  pure $ do
    b <- futureB                         -- IO b
    submit p2 (f2 b) >>= id              -- IO c

passAllAsync :: WorkerPool -> WorkerPool -> (a -> IO b) -> (b -> IO c) -> [a] -> IO [c]
passAllAsync p1 p2 f1 f2 xs = do
  futures <- mapM (passThroughAsync p1 p2 f1 f2) xs  -- [IO c]
  sequence futures  -- IO [c]

passNestedAsync :: WorkerPool -> WorkerPool -> (a -> IO b) -> (b -> IO c) -> [[a]] -> IO [[c]]
passNestedAsync p1 p2 f1 f2 xss = do
  futuresNested <- mapM (mapM (passThroughAsync p1 p2 f1 f2)) xss  -- [[IO c]]
  mapM sequence futuresNested  -- IO [[c]]

