{-# LANGUAGE OverloadedStrings #-}

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
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T

-- | Parse a single log line in Apache Combined Log Format
-- Format: IP - - [timestamp] "METHOD path HTTP/version" status size "referer" "user-agent" "other"
parseLogLine :: String -> Maybe LogEntry
parseLogLine line = do
  let pattern = "^([^ ]+) - - \\[([^]]+)\\] \"([A-Z]+) ([^ ]+) HTTP/([0-9.]+)\" ([0-9]+) ([0-9]+) \"([^\"]+)\" \"([^\"]+)\"" :: String
  let match = line =~ pattern :: [[String]]
  
  case match of
    [[_, ip, timeStr, methodStr, path, httpVer, statusStr, sizeStr, referer, userAgent]] -> do
      timestamp <- parseTimestamp timeStr
      method <- parseMethod methodStr
      status <- readMaybe statusStr
      size <- readMaybe sizeStr
      
      return LogEntry
        { leIpAddress = T.pack ip
        , leTimestamp = timestamp
        , leMethod = method
        , lePath = T.pack path
        , leHttpVersion = T.pack httpVer
        , leStatusCode = status
        , leResponseSize = size
        , leReferrer = if referer == "-" then T.empty else T.pack referer
        , leUserAgent = T.pack userAgent
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
classifyBot :: Text -> BotType
classifyBot ua
  | "Googlebot" `T.isPrefixOf` ua || "Google" `T.isInfixOf` ua = GoogleBot
  | "bingbot" `T.isInfixOf` ua || "BingPreview" `T.isInfixOf` ua = BingBot
  | "AhrefsBot" `T.isInfixOf` ua = AhrefsBot
  | "bot" `T.isInfixOf` lowerUA || "crawler" `T.isInfixOf` lowerUA || 
    "spider" `T.isInfixOf` lowerUA = OtherBot ua
  | otherwise = Browser
  where
    lowerUA = T.toLower ua



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
extractDomain :: Text -> Text
extractDomain url
  | "http://" `T.isPrefixOf` url = T.takeWhile (/= '/') $ T.drop 7 url
  | "https://" `T.isPrefixOf` url = T.takeWhile (/= '/') $ T.drop 8 url
  | otherwise = T.empty

-- | Extract query parameters from path
extractQueryParams :: Text -> [(Text, Text)]
extractQueryParams path =
  case T.break (== '?') path of
    (_, t) | T.null t -> []
    (_, query) -> parseParams (T.tail query)
  where
    parseParams q = map splitParam (T.splitOn "&" q)
    splitParam p = case T.break (== '=') p of
      (k, v) | T.null v -> (k, T.empty)
      (k, v) -> (k, T.tail v)

-- | Check if request is for a static resource
isStaticResource :: Text -> Bool
isStaticResource path =
  any (`T.isSuffixOf` path) staticExtensions
  where
    staticExtensions = [".jpg", ".jpeg", ".png", ".gif", ".css", ".js", 
                       ".ico", ".svg", ".woff", ".woff2", ".ttf", ".eot"]
