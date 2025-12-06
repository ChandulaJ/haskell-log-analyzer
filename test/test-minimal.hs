import Parser
import DataTypes

main :: IO ()
main = do
  putStrLn "=== Minimal Parser Test ==="
  
  -- Test 1: Valid log line
  let validLine = "54.36.149.41 - - [22/Jan/2019:03:56:14 +0330] \"GET /filter/test HTTP/1.1\" 200 30577 \"-\" \"Mozilla/5.0 (compatible; AhrefsBot/6.1)\" \"-\""
  
  case parseLogLine validLine of
    Just entry -> do
      putStrLn "✓ Test 1 PASSED: Valid log line parsed"
      putStrLn $ "  IP: " ++ leIpAddress entry
      putStrLn $ "  Method: " ++ show (leMethod entry)
      putStrLn $ "  Status: " ++ show (leStatusCode entry)
    Nothing -> putStrLn "✗ Test 1 FAILED: Could not parse valid line"
  
  -- Test 2: Invalid log line
  let invalidLine = "this is not a valid log line"
  case parseLogLine invalidLine of
    Nothing -> putStrLn "✓ Test 2 PASSED: Invalid line correctly rejected"
    Just _  -> putStrLn "✗ Test 2 FAILED: Invalid line was parsed"
  
  -- Test 3: Multiple lines
  let multiLine = unlines 
        [ "40.77.167.129 - - [22/Jan/2019:03:56:17 +0330] \"GET /image/14925/productModel/100x100 HTTP/1.1\" 200 1696 \"-\" \"Mozilla/5.0 (compatible; bingbot/2.0)\" \"-\""
        , "66.249.66.194 - - [22/Jan/2019:03:56:18 +0330] \"GET /filter/test HTTP/1.1\" 200 34277 \"-\" \"Mozilla/5.0 (compatible; Googlebot/2.1)\" \"-\""
        ]
  
  let entries = parseLogFile multiLine
  if length entries == 2
    then putStrLn "✓ Test 3 PASSED: Multiple lines parsed correctly (2/2)"
    else putStrLn $ "✗ Test 3 FAILED: Expected 2 entries, got " ++ show (length entries)
  
  -- Test 4: Bot classification
  let googleUA = "Mozilla/5.0 (compatible; Googlebot/2.1)"
  let bingUA = "Mozilla/5.0 (compatible; bingbot/2.0)"
  let browserUA = "Mozilla/5.0 (Windows NT 10.0) Chrome/91.0"
  
  if classifyBot googleUA == GoogleBot
    then putStrLn "✓ Test 4a PASSED: Googlebot detected"
    else putStrLn "✗ Test 4a FAILED: Googlebot not detected"
  
  if classifyBot bingUA == BingBot
    then putStrLn "✓ Test 4b PASSED: Bingbot detected"
    else putStrLn "✗ Test 4b FAILED: Bingbot not detected"
  
  if classifyBot browserUA == Browser
    then putStrLn "✓ Test 4c PASSED: Browser detected"
    else putStrLn "✗ Test 4c FAILED: Browser not detected"
  
  putStrLn "\n=== All Tests Complete ==="
