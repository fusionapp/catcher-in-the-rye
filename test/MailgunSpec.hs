module MailgunSpec where

import           Control.Lens ((#))
import           Data.AffineSpace ((.+^), (.-^))
import           Data.Thyme.Clock (fromSeconds)
import           Data.Thyme.Clock.POSIX (posixTime)
import           Lib (app)
import           Mailgun (APIKey(..), Message(..), UVM(Unverified), verifySignature, signMessage)
import           Network.HTTP.Types (methodPost)
import           Test.Hspec
import qualified Test.Hspec.Wai as W
import           Test.Hspec.Wai hiding (pending)
import           Test.Hspec.Wai.JSON

postJSON path = request methodPost path [("Content-Type", "application/json")]

spec :: Spec
spec = do
  describe "verifySignature" $ do
    let now = posixTime # fromSeconds (1461417255 :: Integer)
        key = APIKey "key-49487a3bbead016679ec337d1a99bcf1"
        message = signMessage key $ Unverified Message
          { timestamp = 1461417255
          , token = "419d10642b5c7c803dc61cbc0c77270b0a3b392d693760dfce"
          , signature = ""
          }
    it "returns Nothing if the signature is invalid base16" $
      verifySignature key now (Unverified message { signature = "aoeu" }) `shouldBe` Nothing
    it "returns Nothing if the signature is too short" $
      verifySignature key now (Unverified message { signature = "aaabb" }) `shouldBe` Nothing
    it "returns Nothing if the signature does not verify" $
      verifySignature key now (Unverified message { token = "aabbccdd" }) `shouldBe` Nothing
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
  with (return app) $ do
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
        postJSON "/mg/mead" [json|{"timestamp": 1234, "token": "a", signature: ""}|] `shouldRespondWith` 403
      it "responds with 500 if an error occured during processing"
        W.pending
      it "responds with 200 if the data was stored successfully"
        W.pending
