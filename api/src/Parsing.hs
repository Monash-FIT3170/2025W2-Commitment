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
  MetaFileChanges(..),
  parseCommitData,
  parseFileDataFromCommit,
  MetaFileChangesDiff(..),
  parseFileDataFromDiff,
  pairByFilePath,
  mergeFileMetaData,
  lastElem
) where

import Text.Read (readMaybe)
import Data.List (isPrefixOf, isSuffixOf, nub, dropWhileEnd, intercalate)
import Data.Char (isSpace, toLower)
import Data.Time (UTCTime)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Control.Applicative (Alternative, empty, (<|>))
import Text.Regex.Posix ((=~))
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)

import Types
import Command
import GitCommands

-- ParseResult
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
successful (CommandResult _   (Just err) _            ) = Error err
successful (CommandResult res _          (Just stdErr)) =
  if failedOutput stdErr || failedOutput res then Error stdErr else Result res
successful (CommandResult res _          _            ) = Result res

parsed :: String -> ParseResult a -> a
parsed _   (Result r)  = r
parsed ""  (Error "")  = error "Got blank string"
parsed ""  (Error msg) = error msg
parsed msg (Error e)   = error $ msg ++ ":\n" ++ e
--                     = error $ msg 
--                     = error $ msg ++ ":\n" ++ e

parsedLists :: String -> [ParseResult a] -> [a]
parsedLists msg = map (parsed msg)

parsedNestedLists :: String -> [[ParseResult a]] -> [[a]]
parsedNestedLists msg = map (parsedLists msg)

toLowerString :: String -> String
toLowerString = map toLower

-- Failure detector
failedOutput :: String -> Bool
failedOutput txt = any (`isPrefixOf` toLowerString txt)
  [ "fatal:"
  , "error:"
  , "could not"
  , "not a git repository"
  , "process exited with code"
  , "encountered error"
  , "process timed out"
  ]

exactText :: String -> ParseResult String
exactText txt = if failedOutput txt then Error txt else Result txt

parseRepoExists :: String -> ParseResult String
parseRepoExists txt = if failedOutput txt then Error txt else Result "repo exists"

-- Repo name from URL
parseRepoName :: String -> ParseResult String
parseRepoName txt
  | failedOutput txt = Error txt
  | otherwise        = maybeToResult "Inside parseRepoName failed to get lastElem"
                    . lastElem
                    . split '/'
                    . clean
                    . trim
                    $ txt
  where
    trim = dropWhileEnd isSpace . dropWhile isSpace
    clean t = if ".git" `isSuffixOf` t then take (length t - 4) t else t

-- Emails
parseContributorEmails :: String -> ParseResult [String]
parseContributorEmails txt
  | failedOutput txt = Error txt
  | otherwise        = Result . nub . lines $ txt

-- Branches
parseRepoBranches :: String -> ParseResult [String]
parseRepoBranches txt
  | failedOutput txt = Error txt
  | otherwise        = Result
    . filter (not . null)
    . map (stripPrefixStar . trim)
    . filter (not . ("->" `isInfixOf`))
    . lines
    $ txt
  where
    trim = dropWhileEnd isSpace . dropWhile isSpace
    stripPrefixStar ('*':' ':rest) = rest
    stripPrefixStar x              = x

-- Commit hashes
parseCommitHashes :: String -> ParseResult [String]
parseCommitHashes txt
  | failedOutput txt = Error txt
  | otherwise        = Result . filter (not . null) . filter validLine . lines $ txt
  where
    validLine l = not ("The system cannot find the path specified." `isPrefixOf` l)

-- Commit metadata
data InbetweenCommitData = InbetweenCommitData
  { ibCommitHash      :: String
  , ibCommitTitle     :: String
  , ibContributorName :: String
  , ibDescription     :: String
  , ibTimestamp       :: String
  , involvedFiles     :: [[String]]
  } deriving (Show, Eq)

parseCommitData :: String -> ParseResult InbetweenCommitData
parseCommitData txt
  | failedOutput txt  = Error txt
  | length blocks < 6 = Error ("has less than 6 blocks: \"" ++ show blocks ++ "\" | txt: \"" ++ txt ++ "\"")
  | otherwise = do
      return InbetweenCommitData
        { ibCommitHash      = trim $ blocks !! 0
        , ibContributorName = trim $ blocks !! 1
        , ibTimestamp       = trim $ blocks !! 2
        , ibCommitTitle     = trim $ blocks !! 3
        , ibDescription     = trim $ blocks !! 4
        , involvedFiles     = parseFileLines (drop 5 blocks)
        }
  where
    blocks = splitOn1 delim txt
    trim   = intercalate "\n" . map (dropWhile isSpace) . lines
    parseFileLines = map words . filter (not . null) . lines . unlines

-- Name-status record
data MetaFileChanges = MetaFileChanges
  { filepathM    :: String
  , oldFilePathM :: String
  , charM        :: ChangeType
  , likenessM    :: Int
  } deriving (Show, Eq)

-- Parse a single name-status line (split into words)
parseFileDataFromCommit :: [String] -> ParseResult MetaFileChanges
parseFileDataFromCommit dataList =
  case dataList of
    (changeStr:rest) -> do
      -- first char is status (A,M,D,R,C), remainder may be percentage for R/C (e.g. "R100")
      let (changeChar:likenessStr) = changeStr
      changeTy <- maybeToResult ("Invalid change type: " ++ [changeChar]) (getChangeType changeChar)

      newFilePathTxt <- maybeToResult "Missing new file path" (lastElem rest)

      let oldFilePathTxt =
            case rest of
              (oldFP:_) | changeChar `elem` ['R','C','M'] -> oldFP
              _                                            -> newFilePathTxt

          likenessInt =
            case rest of
              (_:_) | changeChar `elem` ['R','C'] ->
                let cleaned = filter (/= ',') likenessStr
                in maybe (-1) id (readMaybe cleaned :: Maybe Int)
              _ -> -1

      pure MetaFileChanges
        { filepathM    = newFilePathTxt
        , oldFilePathM = oldFilePathTxt
        , charM        = changeTy
        , likenessM    = likenessInt
        }
    _ -> Error $ "Malformed dataList: " ++ show dataList

-- Diff record
data MetaFileChangesDiff = MetaFileChangesDiff
  { filepathMD     :: String
  , diffMD         :: [String]
  , newLinesMD     :: Int
  , deletedLinesMD :: Int
  } deriving (Show, Eq)

-- Parse a full `git show -p` (or `git diff`) text into per-file diffs
parseFileDataFromDiff :: String -> ParseResult [MetaFileChangesDiff]
parseFileDataFromDiff txt
  | failedOutput txt = Error txt
  | otherwise        = Result (finalize (reverse (maybeAddLast acc curHdr curBody)))
  where
    ls = lines txt

    (acc, curHdr, curBody) = foldl step ([], Nothing, []) ls

    step :: ([ (String,[String]) ], Maybe String, [String]) -> String -> ([ (String,[String]) ], Maybe String, [String])
    step (blocks, mHdr, body) line
      | "diff --git " `isPrefixOf` line =
          let blocks' = case mHdr of
                          Just h  -> (h, reverse body) : blocks
                          Nothing -> blocks
          in (blocks', Just line, [])
      | otherwise = (blocks, mHdr, line:body)

    -- add last block after fold
    maybeAddLast :: [ (String,[String]) ] -> Maybe String -> [String] -> [ (String,[String]) ]
    maybeAddLast blocks (Just h) body = (h, reverse body) : blocks
    maybeAddLast blocks Nothing _     = blocks

    finalize :: [ (String,[String]) ] -> [MetaFileChangesDiff]
    finalize blocks =
      [ let fp = maybe "<unknown>" id (extractFilePath hdr)
            adds = countAdds body
            dels = countDels body
        in MetaFileChangesDiff
              { filepathMD     = fp
              , diffMD         = body
              , newLinesMD     = adds
              , deletedLinesMD = dels
              }
      | (hdr, body) <- blocks
      ]

    -- Extract b/<path> from the diff header
    extractFilePath :: String -> Maybe String
    extractFilePath header =
      case header =~ "^diff --git a/.+ b/(.+)$" :: [[String]] of
        [[_, path]] -> Just path
        _           -> Nothing

    -- count + / - lines but ignore file headers like "+++" and "---"
    countAdds :: [String] -> Int
    countAdds = length . filter (\l -> "+"  `isPrefixOf` l && not ("+++" `isPrefixOf` l))

    countDels :: [String] -> Int
    countDels = length . filter (\l -> "-"  `isPrefixOf` l && not ("---" `isPrefixOf` l))

-- Pair by new file path
pairByFilePath :: [MetaFileChanges] -> [MetaFileChangesDiff] -> [(MetaFileChanges, MetaFileChangesDiff)]
pairByFilePath infos diffs =
  let diffMap = Map.fromList [(filepathMD d, d) | d <- diffs]
  in mapMaybe (\i -> fmap (\d -> (i, d)) (Map.lookup (filepathM i) diffMap)) infos

mergeFileMetaData :: [(MetaFileChanges, MetaFileChangesDiff)] -> [FileChanges]
mergeFileMetaData = map (
    \(changes, diffTxt) -> FileChanges
        (filepathM       changes)
        (oldFilePathM    changes)
        (charM           changes)
        (likenessM       changes)
        (newLinesMD      diffTxt)
        (deletedLinesMD  diffTxt)
        (diffMD          diffTxt)
    )

-- Helpers
splitOn1 :: Eq a => [a] -> [a] -> [[a]]
splitOn1 delim = go
  where
    go s = case breakList delim s of
      Just (before, after) -> before : go after
      Nothing              -> [s]

breakList :: Eq a => [a] -> [a] -> Maybe ([a], [a])
breakList pat xs = case breakOn pat xs of
  Nothing -> Nothing
  Just (a, b) -> Just (a, drop (length pat) b)

breakOn :: Eq a => [a] -> [a] -> Maybe ([a], [a])
breakOn _ [] = Nothing
breakOn pat l@(x:xs)
  | pat `isPrefixOf` l = Just ([], l)
  | otherwise          = fmap (first (x:)) (breakOn pat xs)

first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

split :: Eq a => a -> [a] -> [[a]]
split delim = foldr f [[]]
  where
    f c acc@(x:xs)
      | c == delim = []:acc
      | otherwise  = (c:x):xs

isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf needle haystack = any (needle `isPrefixOf`) (tails haystack)

tails :: [a] -> [[a]]
tails []       = [[]]
tails x@(_:xs) = x : tails xs

lastElem :: [a] -> Maybe a
lastElem [] = Nothing
lastElem xs = Just (last xs)
