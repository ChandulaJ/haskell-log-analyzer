module Parser 
  ( parseLogLine
  , parseLogFile
  , parseMethod
  , parseTimestamp
  , classifyBot
  , parseLogFileWithStats
  , extractDomain
  , extractQueryParams
  , isStaticResource
  ) where

import DataTypes
import Data.Time (UTCTime, parseTimeM, defaultTimeLocale)
import Text.Regex.TDFA ((=~))
import Data.List (isPrefixOf)
import Data.Char (isSpace)

-- | Parse a single log line in Apache Combined Log Format
-- Format: IP - - [timestamp] "METHOD path HTTP/version" status size "referer" "user-agent" "other"
parseLogLine :: String -> Maybe LogEntry
parseLogLine line = do
  let pattern = "^([^ ]+) - - \\[([^]]+)\\] \"([A-Z]+) ([^ ]+) HTTP/([0-9.]+)\" ([0-9]+) ([0-9]+) \"([^\"]+)\" \"([^\"]+)\""
  let match = line =~ pattern :: [[String]]
  
  case match of
    [[_, ip, timeStr, methodStr, path, httpVer, statusStr, sizeStr, referer, userAgent]] -> do
      timestamp <- parseTimestamp timeStr
      method <- parseMethod methodStr
      status <- readMaybe statusStr
      size <- readMaybe sizeStr
      
      return LogEntry
        { leIpAddress = ip
        , leTimestamp = timestamp
        , leMethod = method
        , lePath = path
        , leHttpVersion = httpVer
        , leStatusCode = status
        , leResponseSize = size
        , leReferrer = if referer == "-" then "" else referer
        , leUserAgent = userAgent
        }
    _ -> Nothing

-- | Parse an entire log file (list of lines)
parseLogFile :: String -> [LogEntry]
parseLogFile content = 
  let linesList = lines content
      parsed = map parseLogLine linesList
  in [entry | Just entry <- parsed]

-- | Parse HTTP method from string
parseMethod :: String -> Maybe HttpMethod
parseMethod "GET"     = Just GET
parseMethod "POST"    = Just POST
parseMethod "PUT"     = Just PUT
parseMethod "DELETE"  = Just DELETE
parseMethod "HEAD"    = Just HEAD
parseMethod "OPTIONS" = Just OPTIONS
parseMethod "PATCH"   = Just PATCH
parseMethod other     = Just (UNKNOWN other)

-- | Parse timestamp in Apache log format
-- Format: 22/Jan/2019:03:56:14 +0330
parseTimestamp :: String -> Maybe UTCTime
parseTimestamp timeStr = 
  parseTimeM True defaultTimeLocale "%d/%b/%Y:%H:%M:%S %z" timeStr

-- | Classify user agent into bot types
classifyBot :: String -> BotType
classifyBot ua
  | "Googlebot" `isPrefixOf` ua || "Google" `isInfixOf` ua = GoogleBot
  | "bingbot" `isInfixOf` ua || "BingPreview" `isInfixOf` ua = BingBot
  | "AhrefsBot" `isInfixOf` ua = AhrefsBot
  | "bot" `isInfixOf` lowerUA || "crawler" `isInfixOf` lowerUA || 
    "spider" `isInfixOf` lowerUA = OtherBot ua
  | otherwise = Browser
  where
    lowerUA = map toLower ua
    toLower c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c

-- | Helper to check if substring exists in string (case insensitive)
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
  where
    tails [] = [[]]
    tails xs@(_:xs') = xs : tails xs'

-- | Safe read with Maybe
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
  [(x, rest)] | all isSpace rest -> Just x
  _ -> Nothing

-- | Parse log file and collect statistics about parsing success
parseLogFileWithStats :: String -> ([LogEntry], Int, Int)
parseLogFileWithStats content = 
  let linesList = lines content
      totalLines = length linesList
      parsed = map parseLogLine linesList
      entries = [entry | Just entry <- parsed]
      successCount = length entries
      failedCount = totalLines - successCount
  in (entries, successCount, failedCount)

-- | Extract domain from URL path
extractDomain :: String -> String
extractDomain url
  | "http://" `isPrefixOf` url = takeWhile (/= '/') $ drop 7 url
  | "https://" `isPrefixOf` url = takeWhile (/= '/') $ drop 8 url
  | otherwise = ""

-- | Extract query parameters from path
extractQueryParams :: String -> [(String, String)]
extractQueryParams path =
  case break (== '?') path of
    (_, "") -> []
    (_, _:query) -> parseParams query
  where
    parseParams q = map splitParam (splitOn '&' q)
    splitParam p = case break (== '=') p of
      (k, "") -> (k, "")
      (k, _:v) -> (k, v)
    splitOn _ [] = []
    splitOn c s = case break (== c) s of
      (chunk, []) -> [chunk]
      (chunk, _:rest) -> chunk : splitOn c rest

-- | Check if request is for a static resource
isStaticResource :: String -> Bool
isStaticResource path =
  any (`isSuffixOf` path) staticExtensions
  where
    staticExtensions = [".jpg", ".jpeg", ".png", ".gif", ".css", ".js", 
                       ".ico", ".svg", ".woff", ".woff2", ".ttf", ".eot"]
    isSuffixOf suffix str = 
      let lenS = length suffix
          lenStr = length str
      in lenStr >= lenS && drop (lenStr - lenS) str == suffix
