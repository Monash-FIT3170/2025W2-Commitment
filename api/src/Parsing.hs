{-# LANGUAGE InstanceSigs #-}

module Parsing (
  ParseResult(..),
  maybeToResult,
  successful,
  parsed,
  parsedLists,
  parsedNestedLists,
  failedOutput,
  exactText,
  parseRepoExists,
  parseRepoName,
  parseContributorEmails,
  parseRepoBranches,
  parseCommitHashes,
  InbetweenCommitData(..),
  parseCommitData,
  extractCommitData,
  parseFileDataFromCommit,
  lastElem
) where

import Text.Read (readMaybe)
import Data.List (isPrefixOf, isSuffixOf, nub, dropWhileEnd, intercalate)
import Data.Char (isSpace)
import Data.Time (UTCTime)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Control.Applicative (Alternative, empty, (<|>))

import Threading
import Types
import Command

-- define a type for the parsing result for each of the parsers
data ParseResult a = Result a | Error String
  deriving (Show)

instance Eq a => Eq (ParseResult a) where
  (==) :: ParseResult a -> ParseResult a -> Bool
  (==) (Result a) (Result b) = a == b
  (==) (Error s1) (Error s2) = s1 == s2
  (==) _ _                   = False

instance Functor ParseResult where
  fmap :: (a -> b) -> ParseResult a -> ParseResult b
  fmap f (Result a) = Result (f a)
  fmap _ (Error s)  = Error s

instance Applicative ParseResult where
  pure :: a -> ParseResult a
  pure = Result
  (<*>) :: ParseResult (a -> b) -> ParseResult a -> ParseResult b
  (<*>) (Result f) p = f <$> p
  (<*>) (Error s) _  = Error s

instance Monad ParseResult where
  (>>=) (Result x) f = f x
  (>>=) (Error e)  _ = Error e

instance MonadFail ParseResult where
  fail = Error

instance Alternative ParseResult where
  empty :: ParseResult a
  empty = Error ""

  (<|>) :: ParseResult a -> ParseResult a -> ParseResult a
  (<|>) (Result a) _ = Result a
  (<|>) _ (Result b) = Result b
  (<|>) (Error s1) _ = Error s1

maybeToResult :: String -> Maybe a -> ParseResult a
maybeToResult _ (Just a)  = Result a
maybeToResult msg Nothing = Error msg

successful :: CommandResult -> ParseResult String
successful (CommandResult _   (Just err) (Just stdErr)) = Error $ err ++ ":\n" ++ stdErr
successful (CommandResult _   (Just err) _)             = Error err
successful (CommandResult res _ (Just stdErr))          = if null res then Result stdErr else Result res
successful (CommandResult res _ _)                      = Result res

parsed :: String -> ParseResult a -> a
parsed _ (Result r)   = r
parsed "" (Error "")  = error "Got blank string"
parsed "" (Error msg) = error msg
parsed msg (Error e)  = error $ msg ++ ":\n" ++ e

parsedLists :: String -> [ParseResult a] -> [a]
parsedLists msg = map (parsed msg)

parsedNestedLists :: String -> [[ParseResult a]] -> [[a]]
parsedNestedLists msg = map (parsedLists msg)

-- | Returns true if the output indicates a failed command
failedOutput :: String -> Bool
failedOutput txt = any (`isPrefixOf` txt) ["fatal:", "error:", "could not", "not a git repository", "Process exited with code"]

-- | Safely extract exact text from command output
exactText :: String -> ParseResult String
exactText txt = if failedOutput txt then Error txt else Result txt

parseRepoExists :: String -> ParseResult String
parseRepoExists txt = if failedOutput txt then Error txt else Result "repo exists"

-- | Parses the remote URL into the repo name
parseRepoName :: String -> ParseResult String
parseRepoName txt
  | failedOutput txt = Error txt
  | otherwise = maybeToResult "Inside parseRepoName failed to get lastElem" . lastElem . split '/' . clean . trim $ txt
  where
    trim = dropWhileEnd isSpace . dropWhile isSpace
    clean t = if ".git" `isSuffixOf` t then take (length t - 4) t else t

-- | Remove duplicates from contributor emails
parseContributorEmails :: String -> ParseResult [String]
parseContributorEmails txt
  | failedOutput txt = Error txt
  | otherwise = Result . nub . lines $ txt

parseRepoBranches :: String -> ParseResult [String]
parseRepoBranches txt
  | failedOutput txt = Error txt
  | otherwise = Result . filter (not . null)
                    . map (stripPrefixStar . trim)
                    . filter (not . ("->" `isInfixOf`))
                    . lines $ txt
  where
    trim = dropWhileEnd isSpace . dropWhile isSpace
    stripPrefixStar ('*':' ':rest) = rest
    stripPrefixStar x = x

parseCommitHashes :: String -> ParseResult [String]
parseCommitHashes txt
  | failedOutput txt = Error txt
  | otherwise = Result . filter (not . null) . filter validLine . lines $ txt
  where
    validLine l = not ("The system cannot find the path specified." `isPrefixOf` l)

-- | Commit metadata parsed from stdout

data InbetweenCommitData = InbetweenCommitData
  { ibCommitHash      :: String
  , ibCommitTitle     :: String
  , ibContributorName :: String
  , ibDescription     :: String
  , ibTimestamp       :: UTCTime
  , involvedFiles     :: [[String]]
  } deriving (Show, Eq)

parseCommitData :: String -> ParseResult InbetweenCommitData
parseCommitData txt
  | failedOutput txt = Error txt
  | length blocks < 6 = Error ("has less than 6 blocks: \"" ++ show blocks ++ "\" | txt: \"" ++ txt ++ "\"")
  | otherwise = do
      ts <- parseTimeM True defaultTimeLocale "%a %b %-d %T %Y %z" (trim $ blocks !! 2)
      return InbetweenCommitData
        { ibCommitHash      = trim $ blocks !! 0
        , ibContributorName = trim $ blocks !! 1
        , ibTimestamp       = ts
        , ibCommitTitle     = trim $ blocks !! 3
        , ibDescription     = trim $ blocks !! 4
        , involvedFiles     = parseFileLines (drop 5 blocks)
        }
  where
    delim = "\\n|||END|||"
    blocks = splitOn delim txt
    trim = intercalate "\n" . map (dropWhile isSpace) . lines

    parseFileLines :: [String] -> [[String]]
    parseFileLines = map words . filter (not . null) . lines . unlines

extractCommitData :: (String -> IO String) -- getFileContents
  -> (String -> IO String)  -- getOldFileContents
  -> [String]
  -> IO (ParseResult(String, String, [String]))
extractCommitData getNew getOld filedata = do
    case filedata of
      ("M":newFile:_) -> do
        newContents <- getNew newFile
        oldContents <- getOld newFile
        return $ Result (newContents, oldContents, filedata)

      ("D":oldFile:_) -> do
        oldContents <- getOld oldFile
        return $ Result ("", oldContents, filedata)
      
      (_:newFile:_) -> do
        newContents <- getNew newFile
        return $ Result (newContents, "", filedata)

      _ -> return $ Error $ "filedata not shaped correctly: " ++ show filedata


parseFileDataFromCommit :: String -> String -> [String] -> ParseResult FileChanges
parseFileDataFromCommit newContents oldContents dataList = do
  case dataList of
    (changeStr:rest) -> do
      let (changeChar:likenessStr) = changeStr

      newFile <- maybeToResult "Missing new file path" (lastElem rest)
      changeType <- maybeToResult ("Invalid change type: " ++ [changeChar]) (getChangeType changeChar)

      extraChange <- case rest of
        (oldFilePathTxt:_) | changeChar `elem` ['R', 'C'] -> do
          let cleanLikeness = filter (/= ',') likenessStr
          case readMaybe cleanLikeness of
            Just likenessInt ->
              case changeChar of 
                'R' -> pure $ Just $ Rename $ RenameData oldFilePathTxt likenessInt
                'C' -> pure $ Just $ Copy $ CopyData oldFilePathTxt likenessInt
                _   -> Error "argument failed for some strange reason inside changeChar"
            Nothing ->
              Error $ "Failed to parse likeness int: " ++ cleanLikeness

        (oldFilePathTxt:_) | changeChar == 'M' ->
          pure $ Just $ Modify $ ModifyData (FileContents oldFilePathTxt oldContents)

        _ -> pure Nothing

      pure $ FileChanges (FileContents newFile newContents) (ChangeData changeType extraChange)

    _ -> Error $ "Malformed dataList: " ++ show dataList

-- Helper functions
splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn delim = go
  where
    go s = case breakList delim s of
      Just (before, after) -> before : go after
      Nothing              -> [s]

-- Breaks list on the first occurrence of a delimiter
breakList :: Eq a => [a] -> [a] -> Maybe ([a], [a])
breakList pat xs = case breakOn pat xs of
  Nothing -> Nothing
  Just (a, b) -> Just (a, drop (length pat) b)

-- Breaks list at first occurrence of a pattern
breakOn :: Eq a => [a] -> [a] -> Maybe ([a], [a])
breakOn _ [] = Nothing
breakOn pat l@(x:xs)
  | pat `isPrefixOf` l = Just ([], l)
  | otherwise = fmap (first (x:)) (breakOn pat xs)

first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

split :: Eq a => a -> [a] -> [[a]]
split delim = foldr f [[]]
  where
    f c acc@(x:xs)
      | c == delim = []:acc
      | otherwise = (c:x):xs

isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf needle haystack = any (needle `isPrefixOf`) (tails haystack)

-- | tails :: [a] -> [[a]]
tails :: [a] -> [[a]]
tails [] = [[]]
tails x@(_:xs) = x : tails xs

-- Simpler alternative to Data.List.last that returns Maybe
lastElem :: [a] -> Maybe a
lastElem [] = Nothing
lastElem xs = Just (last xs)