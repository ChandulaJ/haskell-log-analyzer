{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Processing 
  (
    AnalysisResult(..)
  , countByLevel
  , topIPs
  , filterByService
  , requestsPerEndpoint
  , errorsOverTime
  , sessionize
  , computeAnomalies
  , processLogs
  ) where

import Data.List (sortOn, sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Ord (Down(..), comparing)
import Data.Time (UTCTime, NominalDiffTime, diffUTCTime)
import Control.Parallel.Strategies (using, rseq, parBuffer)
import Control.DeepSeq (NFData(..))

import DataTypes (LogEntry(..), isError, StatusCategory(..), statusCategory)
import Utils (bucketTime)

-- | Result of log analysis combining multiple metrics
data AnalysisResult = AnalysisResult
  { arTotalLines     :: !Int                       -- ^ Total number of log entries
  , arByLevel        :: !(Map StatusCategory Int) -- ^ Count by status category
  , arTopIPs         :: ![(String, Int)]          -- ^ Top IPs by request count
  , arErrorsOverTime :: ![(UTCTime, Int)]         -- ^ Error counts per time bucket
  } deriving (Show, Eq)

-- | NFData instance for parallel evaluation of AnalysisResult
instance NFData AnalysisResult where
  rnf (AnalysisResult t bl ti et) = 
    rnf t `seq` rnf (M.toList bl) `seq` rnf ti `seq` rnf et

-- Core Analysis Functions

-- | Count occurrences of each status category
-- Uses strict fold for memory efficiency
countByLevel :: [LogEntry] -> Map StatusCategory Int
countByLevel = foldl' countEntry M.empty
  where
    countEntry !acc entry =
      let category = statusCategory (leStatusCode entry)
      in M.insertWith (+) category 1 acc

-- | Return the top N IPs sorted by descending request count
topIPs :: Int -> [LogEntry] -> [(String, Int)]
topIPs n entries =
  take n
  . sortBy (comparing (Down . snd))
  . M.toList
  $ ipCounts
  where
    ipCounts :: Map String Int
    ipCounts = foldl' countIP M.empty entries
    
    countIP !acc entry = M.insertWith (+) (leIpAddress entry) 1 acc

-- | Filter entries by path prefix (case-insensitive)
filterByService :: String -> [LogEntry] -> [LogEntry]
filterByService servicePath = filter matchesService
  where
    lowerService = map toLower servicePath
    matchesService entry = 
      lowerService `isPrefixOfCI` lePath entry
    
    -- Case-insensitive prefix check
    isPrefixOfCI :: String -> String -> Bool
    isPrefixOfCI prefix str = 
      map toLower (take (length prefix) str) == prefix
    
    -- Simple toLower for ASCII
    toLower :: Char -> Char
    toLower c
      | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise = c

-- | Generic aggregator for counting endpoints extracted by a function
requestsPerEndpoint :: (LogEntry -> Maybe String) -> [LogEntry] -> Map String Int
requestsPerEndpoint extractor = foldl' aggregate M.empty
  where
    aggregate !acc entry =
      case extractor entry of
        Nothing       -> acc
        Just endpoint -> M.insertWith (+) endpoint 1 acc

-- | Bucket error logs into fixed-size time intervals

errorsOverTime :: NominalDiffTime -> [LogEntry] -> [(UTCTime, Int)]
errorsOverTime interval entries =
  sortOn fst
  . M.toList
  $ bucketCounts
  where
    -- Filter to only error entries (status >= 400)
    errorEntries = filter (isError . leStatusCode) entries
    
    -- Count errors per bucket using strict fold
    bucketCounts :: Map UTCTime Int
    bucketCounts = foldl' countInBucket M.empty errorEntries
    
    countInBucket !acc entry =
      let bucket = bucketTime interval (leTimestamp entry)
      in M.insertWith (+) bucket 1 acc

-- | Group logs by IP into sessions
-- A session ends when the time gap between consecutive logs exceeds the timeout

sessionize :: NominalDiffTime -> [LogEntry] -> [[LogEntry]]
sessionize timeout entries =
  -- Process each IP's entries with parallel evaluation strategy
  -- Using parBuffer for streaming parallel evaluation without NFData requirement
  concatMap sessionizeIP groupedByIP `using` parBuffer 100 rseq
  where
    -- Group entries by IP address
    groupedByIP :: [[LogEntry]]
    groupedByIP = 
      map snd
      . M.toList
      . foldl' groupByIP M.empty
      $ entries
    
    groupByIP !acc entry = 
      M.insertWith (++) (leIpAddress entry) [entry] acc
    
    -- Split a single IP's entries into sessions
    sessionizeIP :: [LogEntry] -> [[LogEntry]]
    sessionizeIP [] = []
    sessionizeIP ipEntries =
      let sorted = sortOn leTimestamp ipEntries
      in splitSessions sorted
    
    -- Split sorted entries into sessions based on timeout
    splitSessions :: [LogEntry] -> [[LogEntry]]
    splitSessions [] = []
    splitSessions (x:xs) = go [x] xs
      where
        go currentSession [] = [reverse currentSession]
        go currentSession@(latest:_) (next:rest)
          | gap > timeout = reverse currentSession : go [next] rest
          | otherwise     = go (next:currentSession) rest
          where
            gap = diffUTCTime (leTimestamp next) (leTimestamp latest)
        go [] _ = []  -- Should never happen, but for completeness

-- | Basic anomaly detection

computeAnomalies :: [LogEntry] -> [String]
computeAnomalies entries =
  highTrafficAnomalies ++ highErrorAnomalies
  where
    -- Thresholds
    trafficThreshold = 100
    errorThreshold = 10
    
    -- Count total requests per IP
    ipCounts :: Map String Int
    ipCounts = foldl' countIP M.empty entries
      where
        countIP !acc entry = M.insertWith (+) (leIpAddress entry) 1 acc
    
    -- Count errors per IP
    ipErrorCounts :: Map String Int
    ipErrorCounts = foldl' countError M.empty entries
      where
        countError !acc entry
          | isError (leStatusCode entry) = 
              M.insertWith (+) (leIpAddress entry) 1 acc
          | otherwise = acc
    
    -- Detect high traffic anomalies
    highTrafficAnomalies :: [String]
    highTrafficAnomalies =
      [ "High traffic from IP " ++ ip ++ ": " ++ show count ++ " requests"
      | (ip, count) <- M.toList ipCounts
      , count > trafficThreshold
      ]
    
    -- Detect high error rate anomalies
    highErrorAnomalies :: [String]
    highErrorAnomalies =
      [ "High error rate from IP " ++ ip ++ ": " ++ show count ++ " errors"
      | (ip, count) <- M.toList ipErrorCounts
      , count > errorThreshold
      ]

-- | Process logs and combine all analysis into a single result
-- Uses parallel evaluation where beneficial
processLogs :: [LogEntry] -> AnalysisResult
processLogs entries = AnalysisResult
  { arTotalLines     = totalLines
  , arByLevel        = levelCounts
  , arTopIPs         = topIPsList
  , arErrorsOverTime = errorBuckets
  }
  where
    -- Compute all metrics (some can be parallelized)
    -- Using strict evaluation to force computation
    !totalLines = length entries
    
    -- Count by level
    !levelCounts = countByLevel entries
    
    -- Top 10 IPs
    !topIPsList = topIPs 10 entries
    
    -- Errors bucketed by hour (3600 seconds)
    !errorBuckets = errorsOverTime 3600 entries