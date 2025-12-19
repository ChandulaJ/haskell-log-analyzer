module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import qualified Data.Map.Strict as M
import Text.Printf (printf)

import Parser (parseLogFileWithStats)
import Processing 
  ( topIPs
  , errorsOverTime
  , sessionize
  , computeAnomalies
  , countByLevel
  )

main :: IO ()
main = do
  args <- getArgs
  
  case args of
    [] -> do
      hPutStrLn stderr "Usage: server-log-analyzer-exe <log-file>"
      hPutStrLn stderr "Example: server-log-analyzer-exe data/sample.log"
      exitFailure
    
    (filePath:_) -> do
      putStrLn $ "=== Server Log Analyzer ==="
      putStrLn $ "Analyzing: " ++ filePath
      putStrLn ""
      
      -- Read and parse the log file
      content <- readFile filePath
      let (entries, successCount, failedCount) = parseLogFileWithStats content
      
      -- Show parsing results
      putStrLn $ "Parsing Results:"
      putStrLn $ "  Total lines: " ++ show (successCount + failedCount)
      putStrLn $ "  Successfully parsed: " ++ show successCount
      putStrLn $ "  Failed to parse: " ++ show failedCount
      putStrLn ""
      
      if null entries
        then putStrLn "No valid log entries found."
        else do
          -- Display comprehensive analysis
          putStrLn $ "=== Analysis Results ==="
          putStrLn ""
          
          -- Total requests
          putStrLn $ "Total Requests: " ++ show (length entries)
          putStrLn ""
          
          -- Requests by log level
          putStrLn "Requests by Log Level:"
          let levelCounts = countByLevel entries
          mapM_ (\(level, count) -> 
            putStrLn $ "  " ++ show level ++ ": " ++ show count ++ 
                      " (" ++ printf "%.1f%%" (100.0 * fromIntegral count / fromIntegral (length entries) :: Double) ++ ")"
            ) (M.toList levelCounts)
          putStrLn ""
          
          -- Top 10 IPs
          putStrLn "Top 10 IPs by Request Count:"
          let top10 = topIPs 10 entries
          mapM_ (\(ip, count) -> 
            putStrLn $ "  " ++ ip ++ ": " ++ show count ++ " requests"
            ) top10
          putStrLn ""
          
          -- Errors over time (hourly buckets)
          let errorBuckets = errorsOverTime 3600 entries
          if null errorBuckets
            then putStrLn "No errors found in the logs."
            else do
              putStrLn $ "Errors Over Time (hourly buckets - showing first 10):"
              mapM_ (\(time, count) -> 
                putStrLn $ "  " ++ show time ++ ": " ++ show count ++ " errors"
                ) (take 10 errorBuckets)
              putStrLn ""
          
          -- Session analysis (30-minute timeout)
          let sessions = sessionize 1800 entries  -- 30 minutes
          let sessionSizes = map length sessions
          let avgSessionSize = if null sessions 
                                 then 0.0 
                                 else fromIntegral (sum sessionSizes) / fromIntegral (length sessions) :: Double
          putStrLn $ "Session Analysis (30-minute timeout):"
          putStrLn $ "  Total sessions: " ++ show (length sessions)
          putStrLn $ "  Average requests per session: " ++ printf "%.2f" avgSessionSize
          putStrLn $ "  Largest session: " ++ show (if null sessionSizes then 0 else maximum sessionSizes) ++ " requests"
          putStrLn ""
          
          -- Anomaly detection
          let anomalies = computeAnomalies entries
          if null anomalies
            then putStrLn "No anomalies detected."
            else do
              putStrLn $ "Anomalies Detected (" ++ show (length anomalies) ++ "):"
              mapM_ (\anomaly -> putStrLn $ "  - " ++ anomaly) anomalies
          
          putStrLn ""
          putStrLn "=== Analysis Complete ==="
