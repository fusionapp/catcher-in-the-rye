import Test.Hspec

import qualified MailgunSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Mailgun" MailgunSpec.spec
