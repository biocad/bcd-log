{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module System.BCD.Log.Types
  (
    Milliseconds
  , Level (..)
  , Log (..)
  ) where

import           Control.DeepSeq (NFData)
import           Data.Text       (Text)
import           GHC.Generics    (Generic)

type Milliseconds = Int

data Level = DEBUG | INFO | WARNING | ERROR | CRITICAL
  deriving (Generic, Show, Read, NFData)

data Log = Log { datetime  :: Text
               , timestamp :: Milliseconds
               , level     :: Level
               , app       :: Text
               , msg       :: Text
               }
  deriving (Generic, Show, Read, NFData)
