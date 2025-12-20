{-# LANGUAGE OverloadedStrings #-}

module Utils 
  ( bucketTime
  ) where

import Data.Time (UTCTime, NominalDiffTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)

-- Time Utilities

-- | Bucket a timestamp to the nearest interval boundary (rounding down)
-- 
-- Example: With a 1-hour interval, 14:37:22 becomes 14:00:00
-- This is a general-purpose function that can bucket to any interval
bucketTime :: NominalDiffTime -> UTCTime -> UTCTime
bucketTime interval time =
  let posixTime = utcTimeToPOSIXSeconds time
      intervalSecs = realToFrac interval :: Double
      bucketNum = floor (realToFrac posixTime / intervalSecs) :: Integer
  in posixSecondsToUTCTime (fromIntegral bucketNum * interval)
