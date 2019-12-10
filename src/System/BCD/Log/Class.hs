{-# LANGUAGE MonoLocalBinds #-}

module System.BCD.Log.Class
  ( MonadBCDLog(..)
  , WithBCDLog
  ) where

import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Reader.Class (MonadReader, asks)
import           Data.Has                   (Has (..))
import           Data.Text                  (Text)
import           GHC.Stack                  (HasCallStack, withFrozenCallStack)
import           System.Log.FastLogger      (LoggerSet, pushLogStrLn, toLogStr)

import           System.BCD.Log.Formatting
import           System.BCD.Log.Instances   ()
import           System.BCD.Log.Types

-- | MTL-style class for logging.
--
-- There is an instance of this class for all 'MonadReader's that have a @fast-logger@'s
-- 'LoggerSet' in their env (via 'Has').
--
-- All methods in this class preserve call stack information from their call site.
--
class MonadBCDLog m where
  -- | General method to log a message with any 'Level'.
  logMsg
    :: HasCallStack
    => Text  -- ^ Application name
    -> Level -- ^ Log level
    -> Text  -- ^ Log message
    -> m ()

  {-# INLINE logDebug #-}
  logDebug :: HasCallStack => Text -> Text -> m ()
  logDebug a = withFrozenCallStack $ logMsg a DEBUG

  {-# INLINE logInfo #-}
  logInfo :: HasCallStack => Text -> Text -> m ()
  logInfo a = withFrozenCallStack $ logMsg a INFO

  {-# INLINE logWarning #-}
  logWarning :: HasCallStack => Text -> Text -> m ()
  logWarning a = withFrozenCallStack $ logMsg a WARNING

  {-# INLINE logError #-}
  logError :: HasCallStack => Text -> Text -> m ()
  logError a = withFrozenCallStack $ logMsg a ERROR

  {-# INLINE logCritical #-}
  logCritical :: HasCallStack => Text -> Text -> m ()
  logCritical a = withFrozenCallStack $ logMsg a CRITICAL

-- | Use this constraint instead of plain 'MonadBCDLog' when writing functions
-- that need logging to correctly pass the call stack.
--
type WithBCDLog m = (HasCallStack, MonadBCDLog m)

-- NOTE: WithBCDLog triggers a warning "-Wsimplifiable-class-constraints" when compiled withour
-- MonoLocalBinds extension.

-- | This instance needs @{-\# OVERLAPPING \#-}@ pragma because GHC complains
-- about overlapping instances in some use cases.
--
instance {-# OVERLAPPING #-} (MonadIO m, MonadReader env m, Has LoggerSet env) => MonadBCDLog m where
  {-# INLINE logMsg #-}
  logMsg app level msg = withFrozenCallStack $ do
      logRecord <- constructLogRecord app level msg
      loggerSet <- asks getter
      liftIO $ pushLogStrLn loggerSet $ toLogStr logRecord
