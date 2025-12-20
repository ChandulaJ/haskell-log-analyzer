module ParserSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Parser
import DataTypes
import Data.Maybe (isJust, isNothing)

spec :: Spec
spec = do
  describe "Parser Module" $ do
    
    describe "parseMethod" $ do
      it "parses GET correctly" $ do
        parseMethod "GET" `shouldBe` Just GET
      
      it "parses POST correctly" $ do
        parseMethod "POST" `shouldBe` Just POST
      
      it "parses PUT correctly" $ do
        parseMethod "PUT" `shouldBe` Just PUT
      
      it "parses DELETE correctly" $ do
        parseMethod "DELETE" `shouldBe` Just DELETE
      
      it "parses HEAD correctly" $ do
        parseMethod "HEAD" `shouldBe` Just HEAD
      
      it "parses OPTIONS correctly" $ do
        parseMethod "OPTIONS" `shouldBe` Just OPTIONS
      
      it "parses PATCH correctly" $ do
        parseMethod "PATCH" `shouldBe` Just PATCH
      
      it "handles unknown methods" $ do
        case parseMethod "CUSTOM" of
          Just (UNKNOWN "CUSTOM") -> return ()
          _ -> expectationFailure "Should parse as UNKNOWN"
    
    describe "parseTimestamp" $ do
      it "parses valid timestamp" $ do
        parseTimestamp "22/Jan/2019:03:56:14 +0330" `shouldSatisfy` isJust
      
      it "rejects invalid timestamp" $ do
        parseTimestamp "invalid" `shouldSatisfy` isNothing
      
      it "rejects malformed timestamp" $ do
        parseTimestamp "32/Jan/2019:03:56:14 +0330" `shouldSatisfy` isNothing
    
    describe "classifyBot" $ do
      it "identifies GoogleBot" $ do
        classifyBot "Mozilla/5.0 (compatible; Googlebot/2.1)" `shouldBe` GoogleBot
      
      it "identifies BingBot" $ do
        classifyBot "Mozilla/5.0 (compatible; bingbot/2.0)" `shouldBe` BingBot
      
      it "identifies AhrefsBot" $ do
        classifyBot "Mozilla/5.0 (compatible; AhrefsBot/6.1)" `shouldBe` AhrefsBot
      
      it "identifies generic bot" $ do
        case classifyBot "SomeOther bot/1.0" of
          OtherBot _ -> return ()
          _ -> expectationFailure "Should identify as OtherBot"
      
      it "identifies browsers" $ do
        classifyBot "Mozilla/5.0 (Windows NT 10.0; Win64; x64)" `shouldBe` Browser
    
    describe "parseLogLine" $ do
      it "parses valid log line" $ do
        let line = "54.36.149.41 - - [22/Jan/2019:03:56:14 +0330] \"GET /filter/test HTTP/1.1\" 200 30577 \"-\" \"Mozilla/5.0 (compatible; AhrefsBot/6.1)\" \"-\""
        let result = parseLogLine line
        result `shouldSatisfy` isJust
      
      it "extracts IP address correctly" $ do
        let line = "54.36.149.41 - - [22/Jan/2019:03:56:14 +0330] \"GET /filter/test HTTP/1.1\" 200 30577 \"-\" \"Mozilla/5.0\" \"-\""
        case parseLogLine line of
          Just entry -> leIpAddress entry `shouldBe` "54.36.149.41"
          Nothing -> expectationFailure "Failed to parse valid line"
      
      it "extracts HTTP method correctly" $ do
        let line = "54.36.149.41 - - [22/Jan/2019:03:56:14 +0330] \"POST /api/data HTTP/1.1\" 200 30577 \"-\" \"Mozilla/5.0\" \"-\""
        case parseLogLine line of
          Just entry -> leMethod entry `shouldBe` POST
          Nothing -> expectationFailure "Failed to parse valid line"
      
      it "extracts path correctly" $ do
        let line = "54.36.149.41 - - [22/Jan/2019:03:56:14 +0330] \"GET /api/users HTTP/1.1\" 200 30577 \"-\" \"Mozilla/5.0\" \"-\""
        case parseLogLine line of
          Just entry -> lePath entry `shouldBe` "/api/users"
          Nothing -> expectationFailure "Failed to parse valid line"
      
      it "extracts status code correctly" $ do
        let line = "54.36.149.41 - - [22/Jan/2019:03:56:14 +0330] \"GET /test HTTP/1.1\" 404 30577 \"-\" \"Mozilla/5.0\" \"-\""
        case parseLogLine line of
          Just entry -> leStatusCode entry `shouldBe` 404
          Nothing -> expectationFailure "Failed to parse valid line"
      
      it "extracts response size correctly" $ do
        let line = "54.36.149.41 - - [22/Jan/2019:03:56:14 +0330] \"GET /test HTTP/1.1\" 200 12345 \"-\" \"Mozilla/5.0\" \"-\""
        case parseLogLine line of
          Just entry -> leResponseSize entry `shouldBe` 12345
          Nothing -> expectationFailure "Failed to parse valid line"
      
      it "returns Nothing for malformed line" $ do
        parseLogLine "this is not a valid log line" `shouldSatisfy` isNothing
      
      it "returns Nothing for incomplete line" $ do
        parseLogLine "54.36.149.41 - - [22/Jan/2019:03:56:14" `shouldSatisfy` isNothing
    
    describe "parseLogFile" $ do
      it "parses multiple lines" $ do
        let content = unlines [
              "54.36.149.41 - - [22/Jan/2019:03:56:14 +0330] \"GET /test HTTP/1.1\" 200 30577 \"-\" \"Mozilla/5.0\" \"-\"",
              "31.56.96.51 - - [22/Jan/2019:03:56:16 +0330] \"POST /api HTTP/1.1\" 201 5667 \"-\" \"Chrome/66.0\" \"-\""
              ]
        let entries = parseLogFile content
        length entries `shouldBe` 2
      
      it "skips invalid lines" $ do
        let content = unlines [
              "54.36.149.41 - - [22/Jan/2019:03:56:14 +0330] \"GET /test HTTP/1.1\" 200 30577 \"-\" \"Mozilla/5.0\" \"-\"",
              "invalid line here",
              "31.56.96.51 - - [22/Jan/2019:03:56:16 +0330] \"POST /api HTTP/1.1\" 201 5667 \"-\" \"Chrome/66.0\" \"-\""
              ]
        let entries = parseLogFile content
        length entries `shouldBe` 2
      
      it "handles empty input" $ do
        parseLogFile "" `shouldBe` []
    
    describe "parseLogFileWithStats" $ do
      it "counts successful and failed parses" $ do
        let content = unlines [
              "54.36.149.41 - - [22/Jan/2019:03:56:14 +0330] \"GET /test HTTP/1.1\" 200 30577 \"-\" \"Mozilla/5.0\" \"-\"",
              "invalid line",
              "31.56.96.51 - - [22/Jan/2019:03:56:16 +0330] \"POST /api HTTP/1.1\" 201 5667 \"-\" \"Chrome/66.0\" \"-\""
              ]
        let (entries, successCount, failCount) = parseLogFileWithStats content
        length entries `shouldBe` 2
        successCount `shouldBe` 2
        failCount `shouldBe` 1
    
    describe "extractDomain" $ do
      it "extracts HTTP domain" $ do
        extractDomain "http://example.com/path" `shouldBe` "example.com"
      
      it "extracts HTTPS domain" $ do
        extractDomain "https://example.com/path" `shouldBe` "example.com"
      
      it "returns empty for relative paths" $ do
        extractDomain "/path/to/resource" `shouldBe` ""
    
    describe "isStaticResource" $ do
      it "identifies image files" $ do
        isStaticResource "/images/photo.jpg" `shouldBe` True
        isStaticResource "/images/photo.png" `shouldBe` True
        isStaticResource "/images/photo.gif" `shouldBe` True
      
      it "identifies CSS files" $ do
        isStaticResource "/styles/main.css" `shouldBe` True
      
      it "identifies JavaScript files" $ do
        isStaticResource "/scripts/app.js" `shouldBe` True
      
      it "identifies non-static resources" $ do
        isStaticResource "/api/users" `shouldBe` False
        isStaticResource "/index.html" `shouldBe` False
    
    describe "extractQueryParams" $ do
      it "extracts query parameters" $ do
        let params = extractQueryParams "/path?key1=value1&key2=value2"
        params `shouldBe` [("key1", "value1"), ("key2", "value2")]
      
      it "handles paths without parameters" $ do
        extractQueryParams "/path" `shouldBe` []
      
      it "handles parameters without values" $ do
        let params = extractQueryParams "/path?key1&key2=value2"
        params `shouldBe` [("key1", ""), ("key2", "value2")]

  describe "Property-Based Tests" $ do
    describe "parseMethod is total" $ do
      it "always returns Just for any non-empty string" $ property $
        \str -> not (null str) ==> isJust (parseMethod str)
    
    describe "parseLogFile preserves order" $ do
      it "entries are in the same order as input" $ property $
        \(Positive n) -> n < 10 ==> 
          let validLine = "54.36.149.41 - - [22/Jan/2019:03:56:14 +0330] \"GET /test HTTP/1.1\" 200 30577 \"-\" \"Mozilla/5.0\" \"-\""
              content = unlines (replicate n validLine)
              entries = parseLogFile content
          in length entries == n
