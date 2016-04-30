{-|
Module : Mailgun
-}
module Mailgun
  ( APIKey(..)
  , Message(..)
  , UVM(..)
  , UnverifiedMessage
  , verifySignature
  , signMessage
  ) where

import           Control.Lens (review)
import           Control.Monad (guard)
import           Crypto.Hash (SHA256, digestFromByteString)
import           Crypto.MAC.HMAC (HMAC(HMAC), hmac)
import           Data.Aeson (FromJSON(..))
import           Data.Aeson.TH (deriveJSON, defaultOptions)
import           Data.AffineSpace ((.-.))
import           Data.ByteArray (ByteArray, ByteArrayAccess)
import           Data.ByteArray.Encoding (Base(Base16), convertToBase, convertFromBase)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import           Data.Either.Combinators (rightToMaybe)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Data.Thyme.Clock (UTCTime, fromSeconds)
import           Data.Thyme.Clock.POSIX (posixTime)
import           Data.Thyme.Format.Aeson ()

-- | A record representing an email message delivered by Mailgun over HTTP.
data Message = Message
               { timestamp :: Integer
               -- ^ The timestamp attached to the signature.
               , token :: T.Text
               -- ^ A random token unique to this message.
               , signature :: T.Text
               -- ^ The signature of the timestamp and token.
               }
             deriving (Show, Eq)

$(deriveJSON defaultOptions ''Message)

-- | A Mailgun API key.
newtype APIKey = APIKey { unAPIKey :: ByteString }
  deriving (Show, Eq, ByteArrayAccess)

-- | Implementation detail of UnverifiedMessage.
data UVM m = Unverified { unsafeMessage :: m }
  deriving Show

instance FromJSON (UVM Message) where
  parseJSON value = Unverified <$> parseJSON value

-- | A Mailgun message that has not had its signature generated or verified
-- yet. This isn't promotable in GHC 7.10 if we remove the type parameter,
-- hence this arrangement.
type UnverifiedMessage = UVM Message

encodeHex :: ByteArrayAccess b => b -> T.Text
encodeHex = decodeUtf8 . convertToBase Base16

decodeHex :: ByteArray b => T.Text -> Either String b
decodeHex = convertFromBase Base16 . encodeUtf8

secondsToPosix :: Real n => n -> UTCTime
secondsToPosix = review posixTime . fromSeconds

integerToAscii :: Integer -> ByteString
integerToAscii = C8.pack . show

-- | Verify the signature on a Mailgun message.
-- Returns 'Just' the message if the signature is valid, or 'Nothing' if not.
verifySignature :: APIKey
                -- ^ The API key the message should be signed with.
                -> UTCTime
                -- ^ The current time.
                -> UnverifiedMessage
                -- ^ The message to verify.
                -> Maybe Message
                -- ^ The verified message.
verifySignature key now (Unverified message) = do
  let offset = now .-. secondsToPosix (timestamp message)
  guard $ offset < fromSeconds (3600 :: Integer)
  guard $ offset > fromSeconds (-3600 :: Integer)
  signature' <- rightToMaybe . decodeHex . signature $ message
  digest <- digestFromByteString (signature' :: ByteString)
  let expected :: HMAC SHA256
      expected = hmac key (timestamp' <> token')
      timestamp' = integerToAscii . timestamp $ message
      token' = encodeUtf8 . token $ message
  guard $ expected == HMAC digest
  return message

-- | Sign a Mailgun message.
signMessage :: APIKey
            -- ^ The API key to sign the message with.
            -> UnverifiedMessage
            -- ^ The message to sign; the existing signature value will be overwritten.
            -> Message
            -- ^ The signed message.
signMessage key (Unverified message) = message { signature = signature' }
  where timestamp' = integerToAscii . timestamp $ message
        token' = encodeUtf8 . token $ message
        mac :: HMAC SHA256
        mac = hmac key (timestamp' <> token')
        signature' = encodeHex mac
