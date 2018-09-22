{-# OPTIONS_GHC -fno-warn-orphans #-}

module System.BCD.Log.Instances () where

import           Data.Aeson           (FromJSON (..), ToJSON (..))
import           System.BCD.Log.Types (Level (..), Log (..))

instance ToJSON Log
instance FromJSON Log

instance ToJSON Level
instance FromJSON Level
