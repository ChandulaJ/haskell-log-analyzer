module UtilsSpec where

import Test.Hspec
import Test.QuickCheck
import Utils
import Data.Time (UTCTime, parseTimeM, defaultTimeLocale)
import Data.Maybe (fromJust)

spec :: Spec
spec = do
  describe "Time Utilities" $ do
    describe "formatTimestamp" $ do
      it "formats timestamp correctly" $ do
        let time = parseTime "2019-01-22 03:56:14"
        formatTimestamp time `shouldBe` "2019-01-22 03:56:14 UTC"
    
    describe "formatDuration" $ do
      it "formats seconds correctly" $ do
        formatDuration 45 `shouldBe` "45.0s"
      
      it "formats minutes correctly" $ do
        formatDuration 125 `shouldBe` "2m 5s"
      
      it "formats hours correctly" $ do
        formatDuration 3665 `shouldBe` "1h 1m 5s"
    
    describe "parseTimeDuration" $ do
      it "parses seconds" $ do
        parseTimeDuration "30s" `shouldBe` Just 30
      
      it "parses minutes" $ do
        parseTimeDuration "5m" `shouldBe` Just 300
      
      it "parses hours" $ do
        parseTimeDuration "2h" `shouldBe` Just 7200
      
      it "returns Nothing for invalid input" $ do
        parseTimeDuration "invalid" `shouldBe` Nothing

  describe "Pretty Formatters" $ do
    describe "formatBytes" $ do
      it "formats bytes correctly" $ do
        formatBytes 512 `shouldBe` "512 B"
      
      it "formats kilobytes correctly" $ do
        formatBytes 1536 `shouldBe` "1.50 KB"
      
      it "formats megabytes correctly" $ do
        formatBytes 5242880 `shouldBe` "5.00 MB"
      
      it "formats gigabytes correctly" $ do
        formatBytes 2147483648 `shouldBe` "2.00 GB"
    
    describe "formatPercentage" $ do
      it "formats percentage correctly" $ do
        formatPercentage 0.7523 `shouldBe` "75.23%"
      
      it "handles 100%" $ do
        formatPercentage 1.0 `shouldBe` "100.00%"
      
      it "handles 0%" $ do
        formatPercentage 0.0 `shouldBe` "0.00%"
    
    describe "formatNumber" $ do
      it "formats small numbers" $ do
        formatNumber 42 `shouldBe` "42"
      
      it "formats thousands" $ do
        formatNumber 1234 `shouldBe` "1,234"
      
      it "formats millions" $ do
        formatNumber 1234567 `shouldBe` "1,234,567"

  describe "CSV/JSON Helpers" $ do
    describe "escapeCSV" $ do
      it "doesn't escape simple strings" $ do
        escapeCSV "hello" `shouldBe` "hello"
      
      it "escapes strings with commas" $ do
        escapeCSV "hello,world" `shouldBe` "\"hello,world\""
      
      it "escapes strings with quotes" $ do
        escapeCSV "hello\"world" `shouldBe` "\"hello\"\"world\""
    
    describe "toCSVRow" $ do
      it "creates CSV row" $ do
        toCSVRow ["Alice", "25", "Engineer"] `shouldBe` "Alice,25,Engineer"
      
      it "escapes fields with special characters" $ do
        toCSVRow ["Alice", "25", "Software, Engineer"] `shouldBe` "Alice,25,\"Software, Engineer\""
    
    describe "escapeJSON" $ do
      it "escapes special characters" $ do
        escapeJSON "hello\"world" `shouldBe` "hello\\\"world"
      
      it "escapes newlines" $ do
        escapeJSON "hello\nworld" `shouldBe` "hello\\nworld"
    
    describe "toJSONString" $ do
      it "creates JSON object" $ do
        toJSONString [("name", "Alice"), ("age", "25")] `shouldBe` "{\"name\":\"Alice\",\"age\":\"25\"}"

  describe "List Utilities" $ do
    describe "safeHead" $ do
      it "returns Just for non-empty list" $ do
        safeHead [1, 2, 3] `shouldBe` Just 1
      
      it "returns Nothing for empty list" $ do
        safeHead ([] :: [Int]) `shouldBe` Nothing
      
      it "is safe (QuickCheck property)" $ property $
        \xs -> safeHead (xs :: [Int]) == if null xs then Nothing else Just (head xs)
    
    describe "safeTail" $ do
      it "returns tail for non-empty list" $ do
        safeTail [1, 2, 3] `shouldBe` [2, 3]
      
      it "returns empty list for empty input" $ do
        safeTail ([] :: [Int]) `shouldBe` []
      
      it "is safe (QuickCheck property)" $ property $
        \xs -> safeTail (xs :: [Int]) == if null xs then [] else tail xs
    
    describe "safeLast" $ do
      it "returns Just for non-empty list" $ do
        safeLast [1, 2, 3] `shouldBe` Just 3
      
      it "returns Nothing for empty list" $ do
        safeLast ([] :: [Int]) `shouldBe` Nothing
    
    describe "chunksOf" $ do
      it "splits list into chunks" $ do
        chunksOf 3 [1..7] `shouldBe` [[1,2,3], [4,5,6], [7]]
      
      it "handles empty list" $ do
        chunksOf 3 ([] :: [Int]) `shouldBe` []
      
      it "handles chunk size larger than list" $ do
        chunksOf 10 [1,2,3] `shouldBe` [[1,2,3]]
      
      it "preserves all elements (QuickCheck property)" $ property $
        \n xs -> n > 0 ==> concat (chunksOf n (xs :: [Int])) == xs

  describe "Statistical Helpers" $ do
    describe "average" $ do
      it "calculates average correctly" $ do
        average [1, 2, 3, 4, 5] `shouldBe` 3.0
      
      it "handles empty list" $ do
        average [] `shouldBe` 0
      
      it "handles single element" $ do
        average [42] `shouldBe` 42.0
    
    describe "median" $ do
      it "calculates median for odd-length list" $ do
        median [1, 3, 5] `shouldBe` 3
      
      it "calculates median for even-length list" $ do
        median [1, 2, 3, 4] `shouldBe` 2.5
      
      it "handles empty list" $ do
        median [] `shouldBe` 0
      
      it "handles unsorted list" $ do
        median [5, 1, 3] `shouldBe` 3
    
    describe "percentile" $ do
      it "calculates 50th percentile (median)" $ do
        percentile 50 [1..100] `shouldBe` 50
      
      it "calculates 95th percentile" $ do
        percentile 95 [1..100] `shouldBe` 95
      
      it "handles edge cases" $ do
        percentile 0 [1..100] `shouldBe` 1
        percentile 100 [1..100] `shouldBe` 100
    
    describe "standardDeviation" $ do
      it "calculates standard deviation" $ do
        let sd = standardDeviation [2, 4, 4, 4, 5, 5, 7, 9]
        sd `shouldSatisfy` (\x -> abs (x - 2.0) < 0.1)
      
      it "returns 0 for empty list" $ do
        standardDeviation [] `shouldBe` 0
      
      it "returns 0 for constant values" $ do
        standardDeviation [5, 5, 5, 5] `shouldBe` 0

  describe "String Utilities" $ do
    describe "truncate'" $ do
      it "doesn't truncate short strings" $ do
        truncate' 20 "Hello" `shouldBe` "Hello"
      
      it "truncates long strings" $ do
        truncate' 10 "Hello World!" `shouldBe` "Hello W..."
      
      it "handles edge case of maxLen <= 3" $ do
        truncate' 3 "Hello" `shouldBe` "Hel"
    
    describe "padLeft" $ do
      it "pads on the left" $ do
        padLeft 5 "42" `shouldBe` "   42"
      
      it "doesn't pad if string is already long enough" $ do
        padLeft 2 "Hello" `shouldBe` "Hello"
    
    describe "padRight" $ do
      it "pads on the right" $ do
        padRight 5 "42" `shouldBe` "42   "
      
      it "doesn't pad if string is already long enough" $ do
        padRight 2 "Hello" `shouldBe` "Hello"
    
    describe "capitalize" $ do
      it "capitalizes first letter" $ do
        capitalize "hello" `shouldBe` "Hello"
      
      it "doesn't change already capitalized" $ do
        capitalize "Hello" `shouldBe` "Hello"
      
      it "handles empty string" $ do
        capitalize "" `shouldBe` ""

-- Helper function to parse time for tests
parseTime :: String -> UTCTime
parseTime = fromJust . parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S"
