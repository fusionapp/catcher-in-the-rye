-- | Mailgun message webhook handling.
module Mailgun
  ( Message(..)
  , MessageBody
  , UVM(..)
  , UnverifiedMessage
  , signMessage
  , verifySignature
  , withMessage
  ) where

import           Control.Lens (view, review)
import           Control.Monad (guard)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Except (throwE)
import           Crypto.Hash (SHA256, digestFromByteString)
import           Crypto.MAC.HMAC (HMAC(HMAC), hmac)
import           Data.AffineSpace ((.-.))
import           Data.ByteArray.Encoding (Base(Base16), convertToBase, convertFromBase)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import           Data.ByteString.Lex.Integral (readDecimal_, packDecimal)
import           Data.Either.Combinators (rightToMaybe)
import           Data.Maybe (fromJust, listToMaybe)
import           Data.Monoid ((<>))
import           Data.Thyme.Clock (UTCTime, fromSeconds, getCurrentTime)
import           Data.Thyme.Clock.POSIX (posixTime)
import           Data.Thyme.Format.Aeson ()
import           Network.Wai.Parse (fileContent)
import           Servant (ServantErr(..), err403, err406)

import           App (AppM, appConfig, configMailgunApiKey)
import           Files (Files, MultiPartData, MultiPartDataT)
import           Mailgun.APIKey (APIKey(..))

-- | Servant capture for a Mailgun message as multipart/form-data.
type MessageBody = Files (Maybe UnverifiedMessage)

-- | A record representing an email message delivered by Mailgun over HTTP.
data Message = Message
               { msgTimestamp :: Integer
               -- ^ The timestamp attached to the signature.
               , msgToken :: ByteString
               -- ^ A random token unique to this message.
               , msgSignature :: ByteString
               -- ^ The signature of the timestamp and token.
               , msgAttachment :: ByteString
               -- ^ The data of the first attachment.
               }
             deriving (Show, Eq)

-- | Implementation detail of UnverifiedMessage. This isn't promotable in GHC
-- 7.10 if we remove the type parameter, hence this weird-looking arrangement.
data UVM m = Unverified { unsafeMessage :: m }
  deriving Show

-- | A Mailgun message that has not had its signature generated or verified.
type UnverifiedMessage = UVM Message

-- | Convert a POSIX timestamp in numerical format to a 'UTCTime'.
secondsToPosix :: Real n => n -> UTCTime
secondsToPosix = review posixTime . fromSeconds

-- | Calculate the expected signature value for a message.
--
-- Returns 'Nothing' if the timestamp is negative.
messageSignature :: APIKey -> Message -> Maybe (HMAC SHA256)
messageSignature key message = do
  timestamp <- packDecimal . msgTimestamp $ message
  return $ hmac key (timestamp <> msgToken message)

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
  let offset = now .-. secondsToPosix (msgTimestamp message)
  guard $ offset < fromSeconds (3600 :: Integer)
  guard $ offset > fromSeconds (-3600 :: Integer)
  signature <- rightToMaybe . convertFromBase Base16 . msgSignature $ message
  digest <- digestFromByteString (signature :: ByteString)
  expected <- messageSignature key message
  guard $ expected == HMAC digest
  return message

-- | Sign a Mailgun message.
signMessage :: APIKey
            -- ^ The API key to sign the message with.
            -> UnverifiedMessage
            -- ^ The message to sign; the existing signature value will be overwritten.
            -> Message
            -- ^ The signed message.
signMessage key (Unverified message) = message { msgSignature = signature }
  where mac = fromJust (messageSignature key message)
        signature = convertToBase Base16 mac

-- | Error to return for an invalid message.
--
-- An HTTP 406 response here tells Mailgun not to retry.
errInvalid :: ServantErr
errInvalid = err406 { errBody = "Invalid message body." }

-- | Error to return for an invalid / missing Mailgun signature.
errSignature :: ServantErr
errSignature = err403 { errBody = "Invalid signature." }

-- | Construct an 'UnverifiedMessage' from multipart/form-data.
multipartToMessage :: MultiPartData -> Maybe UnverifiedMessage
multipartToMessage (params, files) = do
  ts <- readDecimal_ <$> lookup "timestamp" params
  token <- lookup "token" params
  sig <- lookup "signature" params
  attachment <- LBS.toStrict . fileContent . snd <$> listToMaybe files
  return $ Unverified (Message ts token sig attachment)

-- | Create a multipart/form-data handler from a handler that takes a Mailgun
-- message.
withMessage :: (Message -> AppM a)
            -- ^ The handler to lift.
            -> MultiPartDataT (Maybe UnverifiedMessage) -> AppM a
            -- ^ The multipart/form-data handler.
withMessage handler multipart = do
  now <- liftIO getCurrentTime
  key <- view (appConfig . configMailgunApiKey)
  m <- liftIO $ multipart (return . multipartToMessage)
  case m of
    Nothing -> lift (throwE errInvalid)
    Just unsigned -> case verifySignature key now unsigned of
      Nothing -> lift (throwE errSignature)
      Just message -> handler message
