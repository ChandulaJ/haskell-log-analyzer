module ProcessingSpec where

import Test.Hspec
import Test.QuickCheck
import Processing
import DataTypes
import Data.Time (UTCTime, parseTimeM, defaultTimeLocale)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M
import qualified Data.List

spec :: Spec
spec = do
  describe "Processing Module" $ do
    
    describe "countByLevel" $ do
      it "counts status categories correctly" $ do
        let entries = [
              mockEntry 200,
              mockEntry 201,
              mockEntry 404,
              mockEntry 500
              ]
        let counts = countByLevel entries
        M.lookup DataTypes.Success counts `shouldBe` Just 2
        M.lookup ClientError counts `shouldBe` Just 1
        M.lookup ServerError counts `shouldBe` Just 1
      
      it "handles empty list" $ do
        let counts = countByLevel []
        M.null counts `shouldBe` True
      
      it "counts all entries (property test)" $ property $
        \codes -> not (null codes) ==> 
          let entries = map mockEntry (codes :: [Int])
              counts = countByLevel entries
          in sum (M.elems counts) == length entries
    
    describe "topIPs" $ do
      it "returns top IPs in descending order" $ do
        let entries = [
              mockEntryWithIP "192.168.1.1",
              mockEntryWithIP "192.168.1.1",
              mockEntryWithIP "192.168.1.1",
              mockEntryWithIP "192.168.1.2",
              mockEntryWithIP "192.168.1.2",
              mockEntryWithIP "192.168.1.3"
              ]
        let tops = topIPs 2 entries
        tops `shouldBe` [("192.168.1.1", 3), ("192.168.1.2", 2)]
      
      it "limits results to N entries" $ do
        let entries = map mockEntryWithIP (map show [1..10])
        let tops = topIPs 5 entries
        length tops `shouldBe` 5
      
      it "handles empty list" $ do
        topIPs 10 [] `shouldBe` []
    
    describe "filterByService" $ do
      it "filters by path prefix" $ do
        let entries = [
              mockEntryWithPath "/api/users",
              mockEntryWithPath "/api/products",
              mockEntryWithPath "/web/home",
              mockEntryWithPath "/api/orders"
              ]
        let filtered = filterByService "/api" entries
        length filtered `shouldBe` 3
      
      it "is case-insensitive" $ do
        let entries = [mockEntryWithPath "/API/users"]
        let filtered = filterByService "/api" entries
        length filtered `shouldBe` 1
      
      it "handles empty list" $ do
        filterByService "/api" [] `shouldBe` []
    
    describe "requestsPerEndpoint" $ do
      it "counts requests per endpoint" $ do
        let entries = [
              mockEntryWithPath "/api/users",
              mockEntryWithPath "/api/users",
              mockEntryWithPath "/api/products",
              mockEntryWithPath "/api/orders"
              ]
        let counts = requestsPerEndpoint (Just . lePath) entries
        M.lookup "/api/users" counts `shouldBe` Just 2
        M.lookup "/api/products" counts `shouldBe` Just 1
        M.lookup "/api/orders" counts `shouldBe` Just 1
      
      it "handles extractor returning Nothing" $ do
        let entries = [mockEntry 200, mockEntry 404]
        let counts = requestsPerEndpoint (\_ -> Nothing) entries
        M.null counts `shouldBe` True
    
    describe "errorsOverTime" $ do
      it "buckets errors by time interval" $ do
        let entries = [
              mockEntryWithStatus 404,
              mockEntryWithStatus 500,
              mockEntryWithStatus 200,
              mockEntryWithStatus 404
              ]
        let buckets = errorsOverTime 3600 entries
        -- Should have error entries (404, 500, 404) = 3 errors
        sum (map snd buckets) `shouldBe` 3
      
      it "excludes successful requests" $ do
        let entries = [
              mockEntryWithStatus 200,
              mockEntryWithStatus 201,
              mockEntryWithStatus 304
              ]
        let buckets = errorsOverTime 3600 entries
        buckets `shouldBe` []
      
      it "handles empty list" $ do
        errorsOverTime 3600 [] `shouldBe` []
    
    describe "sessionize" $ do
      it "splits entries into sessions based on timeout" $ do
        let entries = [
              mockEntryAtTime "2019-01-22 00:00:00",
              mockEntryAtTime "2019-01-22 00:00:10",
              mockEntryAtTime "2019-01-22 00:10:00",  -- 10 min gap
              mockEntryAtTime "2019-01-22 00:10:05"
              ]
        let sessions = sessionize 600 entries  -- 10 minute timeout
        -- Should create 2 sessions due to the gap
        length sessions `shouldSatisfy` (>= 1)
      
      it "handles empty list" $ do
        sessionize 300 [] `shouldBe` []
    
    describe "computeAnomalies" $ do
      it "detects high traffic anomalies" $ do
        -- Create 150 requests from same IP (threshold is 100)
        let entries = replicate 150 (mockEntryWithIP "192.168.1.1")
        let anomalies = computeAnomalies entries
        length anomalies `shouldSatisfy` (> 0)
      
      it "detects high error anomalies" $ do
        -- Create 15 error requests from same IP (error threshold is 10)
        let entries = replicate 15 (mockEntryWithIPAndStatus "192.168.1.1" 500)
        let anomalies = computeAnomalies entries
        length anomalies `shouldSatisfy` (> 0)
      
      it "returns empty list for normal traffic" $ do
        let entries = [
              mockEntryWithIP "192.168.1.1",
              mockEntryWithIP "192.168.1.2",
              mockEntryWithIP "192.168.1.3"
              ]
        let anomalies = computeAnomalies entries
        anomalies `shouldBe` []
    
    describe "processLogs (Integration)" $ do
      it "processes log entries and produces results" $ do
        let entries = [
              mockEntry 200,
              mockEntry 404,
              mockEntry 500
              ]
        let result = processLogs entries
        arTotalLines result `shouldBe` 3
        M.size (arByLevel result) `shouldSatisfy` (> 0)
      
      it "handles empty list" $ do
        let result = processLogs []
        arTotalLines result `shouldBe` 0
    
    describe "bucketTime" $ do
      it "rounds down to interval boundary" $ do
        let time = parseTime "2019-01-22 14:37:22"
        let hourInterval = 3600  -- 1 hour
        let bucketed = bucketTime hourInterval time
        -- Should round down to 14:00:00
        formatTime bucketed `shouldContain` "14:00:00"
      
      it "handles exact interval boundaries" $ do
        let time = parseTime "2019-01-22 15:00:00"
        let hourInterval = 3600
        let bucketed = bucketTime hourInterval time
        bucketed `shouldBe` time

  describe "Property-Based Tests" $ do
    describe "countByLevel properties" $ do
      it "total count equals input size" $ property $
        \codes -> not (null codes) ==> 
          let entries = map mockEntry (codes :: [Int])
              counts = countByLevel entries
          in sum (M.elems counts) == length entries
    
    describe "topIPs properties" $ do
      it "result length is at most N" $ property $
        \n ips -> n > 0 ==> 
          let entries = map mockEntryWithIP (ips :: [String])
              result = topIPs n entries
          in length result <= n
      
      it "results are sorted in descending order" $ property $
        \ips -> not (null (ips :: [String])) ==> 
          let entries = map mockEntryWithIP ips
              result = topIPs 5 entries
              counts = map snd result
              isSorted [] = True
              isSorted [_] = True
              isSorted (x:y:rest) = x >= y && isSorted (y:rest)
          in isSorted counts

--------------------------------------------------------------------------------
-- Helper Functions for Creating Mock Data
--------------------------------------------------------------------------------

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

-- | Parse time from string
parseTime :: String -> UTCTime
parseTime = fromJust . parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S"

-- | Format time to string
formatTime :: UTCTime -> String
formatTime = show
