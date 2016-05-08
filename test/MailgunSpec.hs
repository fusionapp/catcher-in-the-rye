module MailgunSpec where

import           App (AppConfig(..), mkApp)
import           Control.Lens ((#))
import           Control.Monad.Logger (runStderrLoggingT, filterLogger, LogLevel(LevelDebug))
import           Data.AffineSpace ((.+^), (.-^))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as M
import           Data.Monoid ((<>))
import           Data.Thyme.Clock (fromSeconds, toSeconds')
import           Data.Thyme.Clock.POSIX (posixTime, getPOSIXTime)
import           Lib (serveApp)
import           Mailgun (Message(..), UVM(Unverified), verifySignature, signMessage, integerToAscii)
import           Mailgun.APIKey (APIKey(..))
import           Network.HTTP.Types (methodPost)
import           Network.Wai.Test (SResponse)
import           Test.Hspec
import qualified Test.Hspec.Wai as W
import           Test.Hspec.Wai hiding (pending)

boundary' :: BS.ByteString
boundary' = "01604fba-0c07-45af-82e2-c022cc8b73ab"

boundary :: BS.ByteString
boundary = "--" <> boundary' <> "\r\n"

postMime :: BS.ByteString -> LBS.ByteString -> W.WaiSession SResponse
postMime path =
  request
  methodPost
  path
  [("Content-Type", "multipart/form-data; boundary=" <> boundary')]

key :: APIKey
key = APIKey "key-49487a3bbead016679ec337d1a99bcf1"

testConfig :: AppConfig
testConfig = AppConfig
  { _configMailgunApiKey = key
  , _configConnectionString = ":memory:"
  , _configConnections = 1
  , _configSchedules = M.fromList []
  }

messageToBody :: Message -> LBS.ByteString
messageToBody message =
  LBS.fromStrict $
     boundary
  <> "Content-Disposition: form-data; name=\"timestamp\"\r\n"
  <> "\r\n"
  <> integerToAscii (msgTimestamp message) <> "\r\n"
  <> boundary
  <> "Content-Disposition: form-data; name=\"token\"\r\n"
  <> "\r\n"
  <> msgToken message <> "\r\n"
  <> boundary
  <> "Content-Disposition: form-data; name=\"signature\"\r\n"
  <> "\r\n"
  <> msgSignature message <> "\r\n"
  <> boundary
  <> "Content-Disposition: form-data; name=\"attachment-1\"; filename=\"test.zip\"\r\n"
  <> "Content-Type: application/zip\r\n"
  <> "Content-Length: " <> (C8.pack . show . BS.length) content <> "\r\n"
  <> "\r\n"
  <> content <> "\r\n"
  <> "--" <> boundary' <> "--\r\n"
  where content = LBS.toStrict . msgAttachment $ message

testMessage :: LBS.ByteString -> IO LBS.ByteString
testMessage content = do
  now <- getPOSIXTime
  return . messageToBody . signMessage key $ Unverified Message
        { msgTimestamp = truncate (toSeconds' now)
        , msgToken = "419d10642b5c7c803dc61cbc0c77270b0a3b392d693760dfce"
        , msgSignature = ""
        , msgAttachment = content
        }

spec :: Spec
spec = do
  describe "verifySignature" $ do
    let now = posixTime # fromSeconds (1461417255 :: Integer)
        message = signMessage key $ Unverified Message
          { msgTimestamp = 1461417255
          , msgToken = "419d10642b5c7c803dc61cbc0c77270b0a3b392d693760dfce"
          , msgSignature = ""
          , msgAttachment = ""
          }
    it "returns Nothing if the signature is invalid base16" $
      verifySignature key now (Unverified message { msgSignature = "aoeu" }) `shouldBe` Nothing
    it "returns Nothing if the signature is too short" $
      verifySignature key now (Unverified message { msgSignature = "aaabb" }) `shouldBe` Nothing
    it "returns Nothing if the signature does not verify" $
      verifySignature key now (Unverified message { msgToken = "aabbccdd" }) `shouldBe` Nothing
    it "returns Nothing if the signature verifies but the timestamp is too old" $
      verifySignature key (now .+^ fromSeconds (4000 :: Integer)) (Unverified message) `shouldBe` Nothing
    it "returns Nothing if the signature verifies but the timestamp is too new" $
      verifySignature key (now .-^ fromSeconds (4000 :: Integer)) (Unverified message) `shouldBe` Nothing
    it "returns Just the message if the signature verifies" $
      verifySignature key now (Unverified message) `shouldBe` Just message
    it "returns Just the message if the signature verifies with a recent timestamp" $
      verifySignature key (now .+^ fromSeconds (1000 :: Integer)) (Unverified message) `shouldBe` Just message
    it "returns Just the message if the signature verifies with a slightly future timestamp" $
      verifySignature key (now .-^ fromSeconds (1000 :: Integer)) (Unverified message) `shouldBe` Just message
  with mkApp' $ do
    describe "/mg" $
      it "returns a simple message" $
        get "/mg" `shouldRespondWith` "Mailgun service endpoint." {matchHeaders = ["Content-Type" <:> "text/plain;charset=utf-8"]}
    describe "/mg/unknown" $
      it "does not exist" $
        get "/mg/unknown" `shouldRespondWith` 404
    describe "/mg/mead" $ do
      it "returns a simple message" $
        get "/mg/mead" `shouldRespondWith` "M&M loader endpoint." {matchHeaders = ["Content-Type" <:> "text/plain;charset=utf-8"]}
      it "responds with 403 if the signature does not verify" $
        postMime "/mg/mead" (messageToBody $ Message 0 "" "" "") `shouldRespondWith` 403
      it "responds with 403 if the request could not be parsed" $
        postMime "/mg/mead" "{" `shouldRespondWith` 403
      it "responds with 200 if the data was stored successfully" $ do
        message <- liftIO $ testMessage =<< LBS.readFile "test/fixtures/test.zip"
        postMime "/mg/mead" message `shouldRespondWith` 200
  where mkApp' = runStderrLoggingT . filterLogger (const (> LevelDebug)) $
          serveApp <$> mkApp testConfig
