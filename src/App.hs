-- | The main application code and data types.
module App
  ( App
  , AppM
  , Handler
  , appConfig
  , appConnPool
  , mkApp
  , runDB
  , AppConfig(..)
  , configMailgunApiKey
  , configConnectionString
  , configConnections
  , configSchedules
  , UploadSchedule(..)
  , scheduleSchedule
  , scheduleDestination
  ) where

import Control.Lens (view, makeLenses)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Aeson.TH
import Data.Aeson.Types (FromJSON(..), (.:), camelTo2, withObject)
import Data.Map (Map)
import Data.Text (Text)
import Database.Persist.Sqlite (ConnectionPool, SqlPersistT, createSqlitePool, runSqlPool, runMigration)
import Mailgun.APIKey (APIKey(..))
import Models (migrateAll)
import Servant (ServantErr)
import System.Cron (CronSchedule)
import System.Cron.Parser (parseCronSchedule)

-- | A schedule for uploading a data import to its final destination.
data UploadSchedule = UploadSchedule
  { _scheduleSchedule :: CronSchedule
  , _scheduleDestination :: String
  } deriving (Show, Eq)
makeLenses ''UploadSchedule

instance FromJSON UploadSchedule where
  parseJSON = withObject "UploadSchedule" $
    \o -> UploadSchedule
         <$> (either fail return . parseCronSchedule =<< o .: "schedule")
         <*> o .: "destination"

-- | Configuration for the application.
data AppConfig = AppConfig
  { _configMailgunApiKey :: APIKey
  -- ^ The Mailgun API key; needed to verify signatures.
  , _configConnectionString :: Text
  -- ^ Connection string for connecting to the database.
  , _configConnections :: Int
  -- ^ Number of connections to hold in the DB connection pool.
  , _configSchedules :: Map Text UploadSchedule
  -- ^ The upload schedules.
  } deriving (Show, Eq)

deriveFromJSON defaultOptions {fieldLabelModifier = camelTo2 '_'} ''AppConfig
makeLenses ''AppConfig

-- | The main application data type.
data App = App
  { _appConfig :: AppConfig
  , _appConnPool :: ConnectionPool
  }
makeLenses ''App

-- | A Servant handler.
--
-- The type alias is now present upstream but not yet in the version we're
-- using.
type Handler = ExceptT ServantErr IO

-- | Monad transformer stack for a handler in our app.
type AppM = ReaderT App Handler

-- | Prepare the application to run.
--
-- Currently just starts the DB connection pool and runs migrations.
--mkApp :: AppConfig -> IO App
mkApp :: (MonadIO m, MonadBaseControl IO m, MonadLogger m) => AppConfig -> m App
mkApp config = do
    pool <- createSqlitePool (view configConnectionString config) (view configConnections config)
    runSqlPool (runMigration migrateAll) pool
    return (App config pool)

-- | Run a database transaction in the application's DB connection pool.
runDB :: (MonadIO m, MonadReader App m) => SqlPersistT IO a -> m a
runDB q = do
  pool <- view appConnPool
  liftIO $ runSqlPool q pool
