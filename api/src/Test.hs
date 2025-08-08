module Test where

import Commitment

url1 :: String
url1 = "https://github.com/Densetsu152637/test_repo_for_3170"

url2 :: String
url2 = "https://github.com/Monash-FIT3170/2025W2-Commitment"

testApi :: IO ()
testApi = do
  result <- processUrl url2
  case result of
    Just repoData -> BL.putStrLn (encode repoData)
    Nothing       -> safePrint "No repository data found."

-- Process the URL and return repository data
processUrl :: String -> IO (Maybe RepositoryData)
processUrl repoUrl = do
  notifier <- atomically $ newTBQueue 1000

  _ <- forkIO $ forever $ atomically (readTBQueue notifier) >>= safePrint

  -- Main work
  fetchDataFrom repoUrl notifier