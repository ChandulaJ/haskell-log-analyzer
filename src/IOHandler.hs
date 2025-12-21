module IOHandler 
  ( readLogFile
  , writeReport
  ) where

import System.IO ()
import Control.Exception (catch) -- <--- CHANGED: Removed IOError
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Time (UTCTime)
import Text.Printf (printf)
import qualified Data.Text as T

-- Import your data types
import DataTypes (StatusCategory(..))
import Processing (AnalysisResult(..))

-- | Read the content of a log file
-- Returns an empty string and prints error if file reading fails
readLogFile :: FilePath -> IO String
readLogFile path = do
    putStrLn $ "Reading file: " ++ path
    catch (readFile path) handler
  where
    handler :: IOError -> IO String
    handler e = do
        putStrLn $ "Error reading file: " ++ show e
        return ""

-- | Generate and print a professional analysis report
-- This satisfies the requirement for "outputting results" in a pipeline
writeReport :: AnalysisResult -> Int -> Int -> IO ()
writeReport result successCount failCount = do
    putStrLn "\n============================================"
    putStrLn "       LOG ANALYSIS REPORT"
    putStrLn "============================================"
    
    putStrLn $ printf "Total Lines Processed: %d" (successCount + failCount)
    putStrLn $ printf "Successful Parses:     %d" successCount
    putStrLn $ printf "Failed Parses:         %d" failCount
    putStrLn "--------------------------------------------"
    
    putStrLn "Traffic by Status Category:"
    printMap (arByLevel result)
    
    putStrLn "\nTop 10 IP Addresses:"
    mapM_ (\(ip, count) -> printf "  %-15s : %d requests\n" (T.unpack ip) count) (arTopIPs result)
    
    putStrLn "\nError Distribution (Hourly Buckets):"
    if null (arErrorsOverTime result)
      then putStrLn "  No errors detected."
      else mapM_ printErrorBucket (arErrorsOverTime result)
      
    putStrLn "============================================\n"

-- | Helper to print the Status Category map nicely
printMap :: Map StatusCategory Int -> IO ()
printMap m = do
    let categories = M.toList m
    mapM_ (\(cat, count) -> printf "  %-15s : %d\n" (show cat) count) categories

-- | Helper to format time buckets
printErrorBucket :: (UTCTime, Int) -> IO ()
printErrorBucket (time, count) = 
    printf "  %s : %d errors\n" (show time) count