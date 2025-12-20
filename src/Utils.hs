{-# LANGUAGE OverloadedStrings #-}

module Utils 
  ( -- * Time Utilities
    formatTimestamp
  , parseTimeDuration
  , timeDiffInSeconds
  , formatDuration
  , bucketTimeToHour
  , bucketTimeToDay
  , bucketTime
  
  -- * Pretty Formatters
  , formatBytes
  , formatPercentage
  , formatNumber
  , prettyPrintTable
  , colorizeStatus
  
  -- * CSV/JSON Helpers
  , toCSVRow
  , escapeCSV
  , toJSONString
  , escapeJSON
  
  -- * List Utilities
  , safeHead
  , safeTail
  , safeLast
  , chunksOf
  , groupByKey
  
  -- * Statistical Helpers
  , average
  , median
  , percentile
  , standardDeviation
  
  -- * String Utilities
  , truncate'
  , padLeft
  , padRight
  , capitalize
  ) where

import Data.Time (UTCTime, NominalDiffTime, diffUTCTime, formatTime, defaultTimeLocale)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Data.List (sort, intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Text.Printf (printf)


--------------------------------------------------------------------------------
-- Time Utilities
--------------------------------------------------------------------------------

-- | Format a UTCTime to a human-readable string
-- Example: "2019-01-22 03:56:14 UTC"
formatTimestamp :: UTCTime -> String
formatTimestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z"

-- | Parse a duration string like "5m", "2h", "30s" to NominalDiffTime
parseTimeDuration :: String -> Maybe NominalDiffTime
parseTimeDuration str = case reverse str of
  ('s':rest) -> fmap fromIntegral (readMaybe (reverse rest) :: Maybe Int)
  ('m':rest) -> fmap ((*60) . fromIntegral) (readMaybe (reverse rest) :: Maybe Int)
  ('h':rest) -> fmap ((*3600) . fromIntegral) (readMaybe (reverse rest) :: Maybe Int)
  ('d':rest) -> fmap ((*86400) . fromIntegral) (readMaybe (reverse rest) :: Maybe Int)
  _ -> Nothing
  where
    readMaybe :: Read a => String -> Maybe a
    readMaybe s = case reads s of
      [(x, "")] -> Just x
      _ -> Nothing

-- | Calculate the time difference between two timestamps in seconds
timeDiffInSeconds :: UTCTime -> UTCTime -> Double
timeDiffInSeconds t1 t2 = realToFrac (diffUTCTime t1 t2)

-- | Format a duration in seconds to human-readable string
-- Example: 3665 seconds -> "1h 1m 5s"
formatDuration :: NominalDiffTime -> String
formatDuration seconds
  | totalSecs < 60 = printf "%.1fs" totalSecs
  | totalSecs < 3600 = printf "%dm %ds" mins (secs `mod` 60)
  | otherwise = printf "%dh %dm %ds" hours (mins `mod` 60) (secs `mod` 60)
  where
    totalSecs = realToFrac seconds :: Double
    secs = floor totalSecs :: Int
    mins = secs `div` 60
    hours = mins `div` 60

-- | Bucket a timestamp to the nearest hour (rounding down)
bucketTimeToHour :: UTCTime -> UTCTime
bucketTimeToHour time =
  let posixTime = utcTimeToPOSIXSeconds time
      hourInSeconds = 3600 :: NominalDiffTime
      bucketNum = floor (posixTime / hourInSeconds) :: Integer
  in posixSecondsToUTCTime (fromIntegral bucketNum * hourInSeconds)

-- | Bucket a timestamp to the nearest day (rounding down)
bucketTimeToDay :: UTCTime -> UTCTime
bucketTimeToDay time =
  let posixTime = utcTimeToPOSIXSeconds time
      dayInSeconds = 86400 :: NominalDiffTime
      bucketNum = floor (posixTime / dayInSeconds) :: Integer
  in posixSecondsToUTCTime (fromIntegral bucketNum * dayInSeconds)

-- | Bucket a timestamp to the nearest interval boundary (rounding down)
-- 
-- Example: With a 1-hour interval, 14:37:22 becomes 14:00:00
-- This is a general-purpose function that can bucket to any interval
bucketTime :: NominalDiffTime -> UTCTime -> UTCTime
bucketTime interval time =
  let posixTime = utcTimeToPOSIXSeconds time
      intervalSecs = realToFrac interval :: Double
      bucketNum = floor (realToFrac posixTime / intervalSecs) :: Integer
  in posixSecondsToUTCTime (fromIntegral bucketNum * interval)

--------------------------------------------------------------------------------
-- Pretty Formatters
--------------------------------------------------------------------------------

-- | Format bytes to human-readable size (KB, MB, GB, etc.)
-- Example: 1536 -> "1.50 KB"
formatBytes :: Integer -> String
formatBytes bytes
  | bytes < 1024 = show bytes ++ " B"
  | bytes < 1024^(2::Integer) = printf "%.2f KB" (fromIntegral bytes / 1024 :: Double)
  | bytes < 1024^(3::Integer) = printf "%.2f MB" (fromIntegral bytes / (1024^(2::Integer)) :: Double)
  | otherwise = printf "%.2f GB" (fromIntegral bytes / (1024^(3::Integer)) :: Double)

-- | Format a ratio as a percentage
-- Example: 0.7523 -> "75.23%"
formatPercentage :: Double -> String
formatPercentage ratio = printf "%.2f%%" (ratio * 100)

-- | Format a number with thousands separators
-- Example: 1234567 -> "1,234,567"
formatNumber :: Int -> String
formatNumber n = reverse $ intercalate "," $ chunksOf 3 $ reverse $ show n

-- | Pretty print a table with aligned columns
-- Example: [("Name", "Age"), ("Alice", "25"), ("Bob", "30")]
prettyPrintTable :: [(String, String)] -> String
prettyPrintTable rows =
  let maxLen1 = maximum $ map (length . fst) rows
      maxLen2 = maximum $ map (length . snd) rows
      formatRow (col1, col2) = padRight maxLen1 col1 ++ " | " ++ padRight maxLen2 col2
  in unlines $ map formatRow rows

-- | Colorize status code for terminal output (ANSI colors)
-- Green for 2xx, Yellow for 3xx, Red for 4xx/5xx
colorizeStatus :: Int -> String
colorizeStatus code
  | code >= 200 && code < 300 = "\ESC[32m" ++ show code ++ "\ESC[0m"  -- Green
  | code >= 300 && code < 400 = "\ESC[33m" ++ show code ++ "\ESC[0m"  -- Yellow
  | code >= 400 = "\ESC[31m" ++ show code ++ "\ESC[0m"  -- Red
  | otherwise = show code

--------------------------------------------------------------------------------
-- CSV/JSON Helpers
--------------------------------------------------------------------------------

-- | Convert a list of strings to a CSV row
-- Example: ["Alice", "25", "Engineer"] -> "Alice,25,Engineer"
toCSVRow :: [String] -> String
toCSVRow = intercalate "," . map escapeCSV

-- | Escape a string for CSV format (quotes fields with commas/quotes)
escapeCSV :: String -> String
escapeCSV str
  | ',' `elem` str || '"' `elem` str || '\n' `elem` str =
      "\"" ++ concatMap escapeQuote str ++ "\""
  | otherwise = str
  where
    escapeQuote '"' = "\"\""
    escapeQuote c = [c]

-- | Convert key-value pairs to a simple JSON string
-- Example: [("name", "Alice"), ("age", "25")] -> "{\"name\":\"Alice\",\"age\":\"25\"}"
toJSONString :: [(String, String)] -> String
toJSONString pairs =
  "{" ++ intercalate "," (map formatPair pairs) ++ "}"
  where
    formatPair (k, v) = "\"" ++ escapeJSON k ++ "\":\"" ++ escapeJSON v ++ "\""

-- | Escape a string for JSON format
escapeJSON :: String -> String
escapeJSON = concatMap escape
  where
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape '\n' = "\\n"
    escape '\r' = "\\r"
    escape '\t' = "\\t"
    escape c = [c]

--------------------------------------------------------------------------------
-- List Utilities
--------------------------------------------------------------------------------

-- | Safe head that returns Maybe instead of throwing on empty list
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- | Safe tail that returns empty list instead of throwing
safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_:xs) = xs

-- | Safe last that returns Maybe
safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just (last xs)

-- | Split a list into chunks of size n
-- Example: chunksOf 3 [1,2,3,4,5] -> [[1,2,3],[4,5]]
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Group list items by a key function
-- Example: groupByKey fst [(1,'a'), (2,'b'), (1,'c')] -> Map.fromList [(1,[(1,'a'),(1,'c')]), (2,[(2,'b')])]
groupByKey :: Ord k => (a -> k) -> [a] -> Map k [a]
groupByKey keyFn = foldl' addToGroup M.empty
  where
    addToGroup acc item = M.insertWith (++) (keyFn item) [item] acc

--------------------------------------------------------------------------------
-- Statistical Helpers
--------------------------------------------------------------------------------

-- | Calculate the average of a list of numbers
average :: [Double] -> Double
average [] = 0
average xs = sum xs / fromIntegral (length xs)

-- | Calculate the median of a list of numbers
median :: [Double] -> Double
median [] = 0
median xs =
  let sorted = sort xs
      len = length sorted
      mid = len `div` 2
  in if even len
     then (sorted !! (mid - 1) + sorted !! mid) / 2
     else sorted !! mid

-- | Calculate the nth percentile (0-100)
-- Example: percentile 95 [1..100] -> 95
percentile :: Double -> [Double] -> Double
percentile _ [] = 0
percentile p xs =
  let sorted = sort xs
      len = length sorted
      idx = floor (p / 100.0 * fromIntegral (len - 1)) :: Int
  in sorted !! max 0 (min idx (len - 1))

-- | Calculate the standard deviation
standardDeviation :: [Double] -> Double
standardDeviation [] = 0
standardDeviation xs =
  let avg = average xs
      squaredDiffs = map (\x -> (x - avg) ^ (2::Integer)) xs
      variance = average squaredDiffs
  in sqrt variance

--------------------------------------------------------------------------------
-- String Utilities
--------------------------------------------------------------------------------

-- | Truncate a string to maximum length with ellipsis
-- Example: truncate' 10 "Hello World!" -> "Hello Wo..."
truncate' :: Int -> String -> String
truncate' maxLen str
  | length str <= maxLen = str
  | maxLen <= 3 = take maxLen str
  | otherwise = take (maxLen - 3) str ++ "..."

-- | Pad string on the left to specified width
-- Example: padLeft 5 "42" -> "   42"
padLeft :: Int -> String -> String
padLeft width str = replicate (width - length str) ' ' ++ str

-- | Pad string on the right to specified width
-- Example: padRight 5 "42" -> "42   "
padRight :: Int -> String -> String
padRight width str = str ++ replicate (width - length str) ' '

-- | Capitalize the first letter of a string
-- Example: capitalize "hello" -> "Hello"
capitalize :: String -> String
capitalize [] = []
capitalize (c:cs) = toUpper c : cs
  where
    toUpper ch
      | ch >= 'a' && ch <= 'z' = toEnum (fromEnum ch - 32)
      | otherwise = ch
