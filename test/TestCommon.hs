-- | Common helpers for the tests.
module TestCommon
  ( mkApp'
  , key
  ) where

import           API (serveApp)
import           App (AppConfig(..), mkApp)
import           Control.Monad.Logger (runStderrLoggingT, filterLogger, LogLevel(..))
import qualified Data.Map.Strict as M
import           Mailgun.APIKey (APIKey(..))
import           Network.Wai (Application)

key :: APIKey
key = APIKey "key-49487a3bbead016679ec337d1a99bcf1"

testConfig :: AppConfig
testConfig = AppConfig
  { _configMailgunApiKey = key
  , _configConnectionString = ":memory:"
  , _configConnections = 1
  , _configSchedules = M.fromList []
  , _configHttpPort = Nothing
  , _configHttpsPort = Nothing
  , _configCertificate = Nothing
  }

mkApp' :: IO Application
mkApp' = runStderrLoggingT . filterLogger (const (> LevelDebug)) $
  serveApp <$> mkApp testConfig
