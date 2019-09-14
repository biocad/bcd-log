module System.BCD.Log.Formatting
  ( constructLogRecord
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Text              (Text, pack)
import           GHC.Stack              (CallStack, HasCallStack, SrcLoc (..),
                                         callStack, getCallStack,
                                         withFrozenCallStack)

import           System.BCD.Log.Time    (milliseconds, time)
import           System.BCD.Log.Types   (Level, Log (..))

{-# INLINE constructLogRecord #-}
constructLogRecord :: (MonadIO m, HasCallStack) => Text -> Level -> Text -> m Log
constructLogRecord app level msg = withFrozenCallStack $ do
    datetime  <- time
    timestamp <- milliseconds
    let sourceLoc = Just $ showSourceLoc callStack
    return Log{..}

-- | This function is taken from @co-log@.
showSourceLoc :: CallStack -> Text
showSourceLoc cs = showCallStack
  where
    showCallStack :: Text
    showCallStack = case getCallStack cs of
        []                             -> "<unknown loc>"
        [(name, loc)]                  -> showLoc name loc
        (_, loc) : (callerName, _) : _ -> showLoc callerName loc

    showLoc :: String -> SrcLoc -> Text
    showLoc name SrcLoc{..} =
        pack srcLocModule <> "." <> pack name <> ":" <> pack (show srcLocStartLine)
