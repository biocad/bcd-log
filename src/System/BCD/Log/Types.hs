{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module System.BCD.Log.Types
  (
    Milliseconds
  , AppName
  , Level (..)
  , Log (..)
  ) where

import           Control.DeepSeq (NFData)
import           Data.Text       (Text)
import           GHC.Generics    (Generic)

type Milliseconds = Int

type AppName      = Text

data Level = DEBUG | INFO | WARNING | ERROR | CRITICAL
  deriving (Generic, Show, Read, NFData)

data Log = Log { datetime  :: Text
               , timestamp :: Milliseconds
               , level     :: Level
               , app       :: AppName
               , msg       :: Text
               }
  deriving (Generic, Show, Read, NFData)
