module MailgunSpec where

import           Control.Lens ((#))
import           Data.AffineSpace ((.+^), (.-^))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.ByteString.Lex.Integral (packDecimal)
import           Data.Maybe (fromJust)
import           Data.Monoid ((<>))
import           Data.Thyme.Clock (fromSeconds, toSeconds')
import           Data.Thyme.Clock.POSIX (posixTime, getPOSIXTime)
import           Mailgun (Message(..), UVM(Unverified), verifySignature, signMessage)
import           Network.HTTP.Types (methodPost)
import           Network.Wai.Test (SResponse)
import           Test.Hspec
import qualified Test.Hspec.Wai as W
import           Test.Hspec.Wai hiding (pending)
import           TestCommon (key, mkApp')

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

messageToBody :: Message -> LBS.ByteString
messageToBody message =
  LBS.fromStrict $
     boundary
  <> "Content-Disposition: form-data; name=\"timestamp\"\r\n"
  <> "\r\n"
  <> (fromJust . packDecimal . msgTimestamp) message <> "\r\n"
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
  <> "Content-Length: " <> (fromJust . packDecimal . BS.length . msgAttachment) message <> "\r\n"
  <> "\r\n"
  <> msgAttachment message <> "\r\n"
  <> "--" <> boundary' <> "--\r\n"

testMessage :: BS.ByteString -> IO LBS.ByteString
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
        verify now' message' = verifySignature key now' (Unverified message')
    it "returns Nothing if the signature is invalid base16" $ do
      let message' = message { msgSignature = "aoeu" }
      verify now message' `shouldBe` Nothing
    it "returns Nothing if the signature is too short" $ do
      let message' = message { msgSignature = "aaabb" }
      verify now message' `shouldBe` Nothing
    it "returns Nothing if the signature does not verify" $ do
      let message' = message { msgToken = "aabbccdd" }
      verify now message' `shouldBe` Nothing
    it "returns Nothing if the signature verifies but the timestamp is too old" $ do
      let now' = now .+^ fromSeconds (4000 :: Integer)
      verify now' message `shouldBe` Nothing
    it "returns Nothing if the signature verifies but the timestamp is too new" $ do
      let now' = now .-^ fromSeconds (4000 :: Integer)
      verify now' message `shouldBe` Nothing
    it "returns Just the message if the signature verifies" $
      verify now message `shouldBe` Just message
    it "returns Just the message if the signature verifies with a recent timestamp" $ do
      let now' = now .+^ fromSeconds (1000 :: Integer)
      verify now' message `shouldBe` Just message
    it "returns Just the message if the signature verifies with a slightly future timestamp" $ do
      let now' = now .-^ fromSeconds (1000 :: Integer)
      verify now' message `shouldBe` Just message
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
      it "responds with 406 if the request could not be parsed" $
        postMime "/mg/mead" "{" `shouldRespondWith` 406
      it "responds with 200 if the data was stored successfully" $ do
        message <- liftIO $ testMessage =<< BS.readFile "test/fixtures/test.zip"
        postMime "/mg/mead" message `shouldRespondWith` 200
