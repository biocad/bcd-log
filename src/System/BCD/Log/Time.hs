{-# LANGUAGE OverloadedStrings #-}

module System.BCD.Log.Time
  ( time
  , seconds
  , nanoseconds
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Text              (Text, pack)
import           Data.Time              (defaultTimeLocale, formatTime,
                                         getZonedTime)
import           System.Clock           (Clock (..), getTime, toNanoSecs)

-- | Get time in nice format (example: '2018-09-22T11:53:34+0200')
--
time :: MonadIO m => m Text
time = let rfc3339like = "%FT%T%z"
       in liftIO $ pack . formatTime defaultTimeLocale rfc3339like <$> getZonedTime

-- | Get current time in milliseconds
--
seconds :: MonadIO m => m Int
seconds = (`div` 10^(9::Int)) <$> nanoseconds

-- | Get current time in nanoseconds
--
nanoseconds :: MonadIO m => m Int
nanoseconds = liftIO $ fromIntegral . toNanoSecs <$> getTime Realtime

