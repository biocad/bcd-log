{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}

module System.BCD.Log
  (
    module System.BCD.Log.Types
  , module System.BCD.Log.Time
  , log'
  , debug'
  , info'
  , warning'
  , error'
  , critical'
  ) where

import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Data.Aeson               (encode)
import           Data.ByteString.Lazy     (toStrict)
import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8)
import qualified Data.Text.IO             (putStrLn)
import           System.BCD.Log.Instances ()
import           System.BCD.Log.Time      (milliseconds, time)
import           System.BCD.Log.Types     (AppName, Level (..), Log (..),
                                           Milliseconds)

type CommonLog    m = Level -> AppName -> Text -> m ()
type SpecifiedLog m =          AppName -> Text -> m ()

debug' :: MonadIO m => SpecifiedLog m
debug' = log' DEBUG

info' :: MonadIO m => SpecifiedLog m
info' = log' INFO

warning' :: MonadIO m => SpecifiedLog m
warning' = log' WARNING

error' :: MonadIO m => SpecifiedLog m
error' = log' ERROR

critical' :: MonadIO m => SpecifiedLog m
critical' = log' CRITICAL

log' :: MonadIO m => CommonLog m
log' level app msg = do
    datetime  <- time
    timestamp <- milliseconds
    save . format $ Log {..}
  where
    format :: Log -> Text
    format = decodeUtf8 . toStrict . encode
    
    save :: MonadIO m => Text -> m ()
    save = liftIO . Data.Text.IO.putStrLn
