{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}

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
  , format
  , save
  , MonadBCDLog(..)
  , WithBCDLog
  ) where

import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Data.Aeson               (encode)
import           Data.Text                (Text)
import qualified Data.Text.IO             (putStrLn)
import           System.BCD.Log.Class     (MonadBCDLog (..), WithBCDLog)
import           System.BCD.Log.Instances ()
import           System.BCD.Log.TextLike  (TextLike (..))
import           System.BCD.Log.Time      (milliseconds, time)
import           System.BCD.Log.Types     (Level (..), Log (..), Milliseconds)

type CommonLog    m = forall t t1. (TextLike t, TextLike t1) => Level -> t -> t1 -> m ()
type SpecifiedLog m = forall t t1. (TextLike t, TextLike t1) =>          t -> t1 -> m ()

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
log' level (toText -> app) (toText -> msg) = do
    datetime  <- time
    timestamp <- milliseconds
    save . format $ Log {sourceLoc = Nothing, ..}

format :: Log -> Text
format = toText . encode

save :: MonadIO m => Text -> m ()
save = liftIO . Data.Text.IO.putStrLn
