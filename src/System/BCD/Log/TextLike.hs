{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module System.BCD.Log.TextLike where

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text            as T
import           Data.Text.Encoding   (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy       as TL

-- | This class introduces to reduce dummy convension to text for different text representations.
-- This is anologue to 'Data.Text.Conversions', but not creates Box for 'ByteString'.
--
class TextLike a where
    toText :: a -> T.Text
    fromText :: T.Text -> a

instance TextLike T.Text where
    toText   = id
    fromText = id

instance TextLike String where
    toText   = T.pack
    fromText = T.unpack

instance TextLike TL.Text where
    toText   = TL.toStrict
    fromText = TL.fromStrict

instance TextLike BS.ByteString where
    toText = decodeUtf8
    fromText = encodeUtf8

instance TextLike BSL.ByteString where
    toText = toText . BSL.toStrict
    fromText = BSL.fromStrict . fromText
