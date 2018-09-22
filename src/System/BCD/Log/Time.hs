{-# LANGUAGE OverloadedStrings #-}

module System.BCD.Log.Time
  (
    time
  , milliseconds
  , nanoseconds
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Text              (Text, pack)
import           Data.Time              (defaultTimeLocale, formatTime,
                                         getZonedTime)
import           System.BCD.Log.Types   (Milliseconds)
import           System.Clock           (Clock (..), getTime, toNanoSecs)

-- | Get time in nice format (example: '2018-09-22T11:53:34+0200')
--
time :: MonadIO m => m Text
time = let rfc3339like = "%FT%T%z"
       in liftIO $ pack . formatTime defaultTimeLocale rfc3339like <$> getZonedTime

-- | Get current time in milliseconds
--
milliseconds :: MonadIO m => m Milliseconds
milliseconds = (`div` 10^(6::Int)) <$> nanoseconds

-- | Get current time in nanoseconds
--
nanoseconds :: MonadIO m => m Int
nanoseconds = liftIO $ fromIntegral . toNanoSecs <$> getTime Realtime

