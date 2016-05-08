-- | Data type for Mailgun API keys.
module Mailgun.APIKey
  ( APIKey(..)
  ) where

import Data.Aeson (FromJSON, parseJSON)
import Data.ByteArray (ByteArrayAccess)
import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8)

-- | A Mailgun API key.
newtype APIKey = APIKey { unAPIKey :: ByteString }
  deriving (Show, Eq, ByteArrayAccess)

instance FromJSON APIKey where
  parseJSON value = APIKey . encodeUtf8 <$> parseJSON value
