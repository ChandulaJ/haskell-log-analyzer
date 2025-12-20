module TestHelpers 
  ( -- * Mock Data Creation
    mockEntry
  , mockEntryWithIP
  , mockEntryWithPath
  , mockEntryWithStatus
  , mockEntryWithIPAndStatus
  , mockEntryAtTime
  
  -- * Time Utilities
  , parseTime
  , formatTime
  ) where

import DataTypes (LogEntry(..), HttpMethod(..))
import Data.Time (UTCTime, parseTimeM, defaultTimeLocale)
import Data.Maybe (fromJust)

-- Mock Data Creation Helpers

-- | Create a mock LogEntry with specific status code
mockEntry :: Int -> LogEntry
mockEntry statusCode = LogEntry
  { leIpAddress = "127.0.0.1"
  , leTimestamp = parseTime "2019-01-22 03:56:14"
  , leMethod = GET
  , lePath = "/test"
  , leHttpVersion = "1.1"
  , leStatusCode = statusCode
  , leResponseSize = 1024
  , leReferrer = ""
  , leUserAgent = "Test Agent"
  }

-- | Create a mock LogEntry with specific IP address
mockEntryWithIP :: String -> LogEntry
mockEntryWithIP ip = (mockEntry 200) { leIpAddress = ip }

-- | Create a mock LogEntry with specific path
mockEntryWithPath :: String -> LogEntry
mockEntryWithPath path = (mockEntry 200) { lePath = path }

-- | Create a mock LogEntry with specific status
mockEntryWithStatus :: Int -> LogEntry
mockEntryWithStatus = mockEntry

-- | Create a mock LogEntry with specific IP and status
mockEntryWithIPAndStatus :: String -> Int -> LogEntry
mockEntryWithIPAndStatus ip status = 
  (mockEntry status) { leIpAddress = ip }

-- | Create a mock LogEntry at a specific time
mockEntryAtTime :: String -> LogEntry
mockEntryAtTime timeStr = 
  (mockEntry 200) { leTimestamp = parseTime timeStr }

-- Time Utilities for Tests

-- | Parse time from string for test purposes
parseTime :: String -> UTCTime
parseTime = fromJust . parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S"

-- | Format time to string for test assertions
formatTime :: UTCTime -> String
formatTime = show
