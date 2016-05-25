-- | The main application code and data types.
module App (
  -- * Core application types
    App
  , appConfig
  , appConnPool
  , AppT
  , AppM
  , Handler
  , mkApp
  , runAppT
  , runDB
  -- * Global application configuration
  , AppConfig(..)
  , configMailgunApiKey
  , configConnectionString
  , configConnections
  , configSchedules
  , configHttpPort
  , configHttpsPort
  , configCertificate
  -- ** Upload scheduling
  , UploadSchedule(..)
  , scheduleSchedule
  , scheduleDestinations
  , scheduleNotifications
  ) where

import Control.Lens (Lens', view, makeLensesWith)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Aeson.TH
import Data.Aeson.Types (FromJSON(..), (.:), withObject)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Database.Persist.Sqlite (ConnectionPool, SqlPersistT, createSqlitePool, runSqlPool, runMigrationSilent)
import Servant (ServantErr)
import System.Cron (CronSchedule)
import System.Cron.Parser (parseCronSchedule)

import Mailgun.APIKey (APIKey(..))
import Models (migrateAll)
import Notifications (NotificationTarget)
import Rules (noSigs, jsonOptions)

-- | A schedule for uploading a data import to its final destination.
data UploadSchedule = UploadSchedule
  { _scheduleSchedule :: CronSchedule
  , _scheduleDestinations :: [String]
  , _scheduleNotifications :: [NotificationTarget]
  } deriving (Show, Eq)
makeLensesWith noSigs ''UploadSchedule

instance FromJSON UploadSchedule where
  parseJSON = withObject "UploadSchedule" $
    \o -> UploadSchedule
         <$> (either fail return . parseCronSchedule =<< o .: "schedule")
         <*> o .: "destinations"
         <*> o .: "notifications"

-- | The schedule on which uploads will be performed.
scheduleSchedule :: Lens' UploadSchedule CronSchedule

-- | The destination URIs the data will be uploaded to.
scheduleDestinations :: Lens' UploadSchedule [String]

-- | The targets to notify once the uploads are complete.
scheduleNotifications :: Lens' UploadSchedule [NotificationTarget]

-- | Configuration for the application.
data AppConfig = AppConfig
  { _configMailgunApiKey :: APIKey
  , _configConnectionString :: Text
  , _configConnections :: Int
  , _configSchedules :: Map Text UploadSchedule
  , _configHttpPort :: Maybe Int
  , _configHttpsPort :: Maybe Int
  , _configCertificate :: Maybe FilePath
  } deriving (Show, Eq)
deriveFromJSON jsonOptions ''AppConfig
makeLensesWith noSigs ''AppConfig

-- | The Mailgun API key; needed to verify signatures.
configMailgunApiKey :: Lens' AppConfig APIKey

-- | Connection string for connecting to the database.
configConnectionString :: Lens' AppConfig Text

-- | Number of connections to hold in the DB connection pool.
configConnections :: Lens' AppConfig Int

-- | The upload schedules.
configSchedules :: Lens' AppConfig (Map Text UploadSchedule)

-- | Port to serve HTTP on, or 'Nothing' to use the default.
configHttpPort :: Lens' AppConfig (Maybe Int)

-- | Port to serve HTTPS on, or 'Nothing' to disable.
--
-- If HTTPS is enabled then HTTP will be disabled.
configHttpsPort :: Lens' AppConfig (Maybe Int)

-- | Path to the certificate / private key / chain for TLS.
configCertificate :: Lens' AppConfig (Maybe FilePath)

-- | The main application data type.
data App = App
  { _appConfig :: AppConfig
  , _appConnPool :: ConnectionPool
  }
makeLensesWith noSigs ''App

-- | The application configuration.
appConfig :: Lens' App AppConfig

-- | The application-wide database connection pool.
appConnPool :: Lens' App ConnectionPool

-- | A Servant handler.
--
-- The type alias is now present upstream but not yet in the version we're
-- using.
type Handler = ExceptT ServantErr IO

-- | Monad transformer for our app.
type AppT m = ReaderT App m

-- | Monad transformer stack for a handler in our app.
type AppM = AppT Handler

-- | Build the application from a configuration.
--
-- Currently just starts the DB connection pool and runs migrations. The type
-- of the Monad is generalized to allow running under different LoggerT setups.
mkApp :: (MonadIO m, MonadBaseControl IO m, MonadLogger m) => AppConfig -> m App
mkApp config = do
    pool <- createSqlitePool (view configConnectionString config) (view configConnections config)
    -- Discard the list of migrations run
    _ <- runSqlPool (runMigrationSilent migrateAll) pool
    return (App config pool)

-- | Run an AppT action.
runAppT :: App -> AppT m a -> m a
runAppT app a = runReaderT a app

-- | Run a database transaction in the application's DB connection pool.
runDB :: (MonadIO m, MonadReader App m) => SqlPersistT IO a -> m a
runDB transaction = do
  pool <- view appConnPool
  liftIO $ runSqlPool transaction pool
