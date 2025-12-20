module UtilsSpec (spec) where

import Test.Hspec
import Utils (bucketTime)
import Data.Time (UTCTime, parseTimeM, defaultTimeLocale)
import Data.Maybe (fromJust)

spec :: Spec
spec = do
  describe "Time Utilities" $ do
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

-- Helper function to parse time for tests
parseTime :: String -> UTCTime
parseTime = fromJust . parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S"

-- Helper function to format time
formatTime :: UTCTime -> String
formatTime = show
