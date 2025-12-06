module DataTypes 
  ( LogEntry(..)
  , HttpMethod(..)
  , LogStats(..)
  , TimeWindow(..)
  , StatusCategory(..)
  , BotType(..)
  , statusCategory
  , isError
  ) where

import Data.Time (UTCTime)
-- Data types for log analyzer

-- 54.36.149.41 - - [22/Jan/2019:03:56:14 +0330] "GET /filter/27|13%20%D9%85%DA%AF%D8%A7%D9%BE%DB%8C%DA%A9%D8%B3%D9%84,27|%DA%A9%D9%85%D8%AA%D8%B1%20%D8%A7%D8%B2%205%20%D9%85%DA%AF%D8%A7%D9%BE%DB%8C%DA%A9%D8%B3%D9%84,p53 HTTP/1.1" 200 30577 "-" "Mozilla/5.0 (compatible; AhrefsBot/6.1; +http://ahrefs.com/robot/)" "-"
-- 31.56.96.51 - - [22/Jan/2019:03:56:16 +0330] "GET /image/60844/productModel/200x200 HTTP/1.1" 200 5667 "https://www.zanbil.ir/m/filter/b113" "Mozilla/5.0 (Linux; Android 6.0; ALE-L21 Build/HuaweiALE-L21) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/66.0.3359.158 Mobile Safari/537.36" "-"
-- 31.56.96.51 - - [22/Jan/2019:03:56:16 +0330] "GET /image/61474/productModel/200x200 HTTP/1.1" 200 5379 "https://www.zanbil.ir/m/filter/b113" "Mozilla/5.0 (Linux; Android 6.0; ALE-L21 Build/HuaweiALE-L21) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/66.0.3359.158 Mobile Safari/537.36" "-"
-- 40.77.167.129 - - [22/Jan/2019:03:56:17 +0330] "GET /image/14925/productModel/100x100 HTTP/1.1" 200 1696 "-" "Mozilla/5.0 (compatible; bingbot/2.0; +http://www.bing.com/bingbot.htm)" "-"

-- Log format: IP - - [timestamp] "METHOD path HTTP/version" status size "referer" "user-agent" "other"

data LogEntry = LogEntry
  { leIpAddress :: String,
   leTimestamp :: UTCTime,
   leMethod :: HttpMethod,
   lePath :: String,
   leHttpVersion :: String,
   leStatusCode :: Int,
   leResponseSize :: Int,
   leReferrer :: String,
   leUserAgent :: String
  } deriving (Show, Eq)

data HttpMethod 
  = GET 
  | POST 
  | PUT 
  | DELETE 
  | HEAD 
  | OPTIONS 
  | PATCH
  | UNKNOWN String
  deriving (Show, Eq, Ord)

-- Analysis results
data LogStats = LogStats
  { lsTotalRequests :: Int,
   lsMethodDistribution :: [(HttpMethod, Int)],
   lsStatusDistribution :: [(Int, Int)],
   lsTopPaths :: [(String, Int)],
   lsTopIPs :: [(String, Int)],
   lsErrorRate :: Double,
   lsTotalBandwidth :: Integer,
   lsAvgResponseSize :: Double
  } deriving (Show)

data TimeWindow = Hourly | Daily deriving (Show, Eq)

-- Status code categories
data StatusCategory 
  = Success      -- 2xx
  | Redirection  -- 3xx
  | ClientError  -- 4xx
  | ServerError  -- 5xx
  | Other
  deriving (Show, Eq, Ord)

-- Helper to categorize status codes
statusCategory :: Int -> StatusCategory
statusCategory code
  | code >= 200 && code < 300 = Success
  | code >= 300 && code < 400 = Redirection
  | code >= 400 && code < 500 = ClientError
  | code >= 500 && code < 600 = ServerError
  | otherwise = Other

-- Helper to check if it's an error
isError :: Int -> Bool
isError code = code >= 400

-- User agent classification
data BotType 
  = GoogleBot
  | BingBot
  | AhrefsBot
  | OtherBot String
  | Browser
  deriving (Show, Eq, Ord)