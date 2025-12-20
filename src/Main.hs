module Main where

import System.Environment (getArgs)
import System.IO (hFlush, stdout)

-- Import local modules
import Parser
import Processing
import IOHandler

-- | Main entry point
-- Pipelines: Input (IO) -> Parse (Pure) -> Process (Pure) -> Output (IO)
main :: IO ()
main = do
    -- 1. User Interaction (Required by guidelines)
    putStrLn "--- Haskell Log Analyzer ---"
    filePath <- getFilePath
    
    -- 2. Input Phase (IO)
    content <- readLogFile filePath
    
    if null content
      then putStrLn "Exiting: No content to process."
      else do
        putStrLn "Parsing logs..."
        
        -- 3. Parsing Phase (Pure)
        -- Using parseLogFileWithStats to get parsing metrics
        let (entries, successCount, failCount) = parseLogFileWithStats content
        
        putStrLn $ "Processing " ++ show successCount ++ " log entries..."
        
        -- 4. Processing Phase (Pure)
        -- This is where the heavy lifting happens (parallelized internally)
        let result = processLogs entries
        
        -- 5. Anomaly Detection (Pure)
        -- Demonstrate functional extraction of anomalies
        let anomalies = computeAnomalies entries
        
        -- 6. Output Phase (IO)
        writeReport result successCount failCount
        
        if not (null anomalies)
            then do
                putStrLn "!!! ANOMALIES DETECTED !!!"
                mapM_ (\msg -> putStrLn $ "  [ALERT] " ++ msg) anomalies
            else putStrLn "No specific anomalies detected."

-- | Helper to get file path from args or user input
getFilePath :: IO String
getFilePath = do
    args <- getArgs
    case args of
        (path:_) -> return path
        [] -> do
            putStr "Enter path to log file (e.g., access.log): "
            hFlush stdout
            getLine