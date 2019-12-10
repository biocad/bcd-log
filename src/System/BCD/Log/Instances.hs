{-# OPTIONS_GHC -fno-warn-orphans #-}

module System.BCD.Log.Instances () where

import           Data.Aeson            (FromJSON (..), ToJSON (..),
                                        defaultOptions, encode,
                                        genericParseJSON, genericToEncoding,
                                        genericToJSON, omitNothingFields)
import           System.BCD.Log.Types  (Level (..), Log (..))
import           System.Log.FastLogger (ToLogStr (..))

instance ToJSON Log where
  toJSON     = genericToJSON $ defaultOptions { omitNothingFields = True }
  toEncoding = genericToEncoding $ defaultOptions { omitNothingFields = True }
instance FromJSON Log where
  parseJSON  = genericParseJSON $ defaultOptions { omitNothingFields = True }

instance ToJSON Level where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Level

instance ToLogStr Log where
  toLogStr = toLogStr . encode
