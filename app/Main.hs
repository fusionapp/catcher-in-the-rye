module Main where

import Control.Lens (view, itraverse_)
import Control.Monad.Logger (LoggingT, runStderrLoggingT, filterLogger, LogLevel(..))
import Control.Monad.State (modify)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import Database.Persist.Sqlite (ConnectionPool, runSqlPool)
import Network.Wai.Handler.Warp (run, defaultSettings, setPort)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import Network.Wai.Middleware.RequestLogger (logStdout)
import PayloadTag (fromText)
import System.Cron (Job(Job), Schedule, execSchedule)
import System.Environment (lookupEnv)
import Upload (doUpload)

import App
import API

-- | Run with our standard logging settings.
runLoggingT :: LoggingT IO a -> IO a
runLoggingT = runStderrLoggingT . filterLogger (const (> LevelDebug))

-- | Schedule uploads to a particular destination.
scheduleUpload :: ConnectionPool
               -- ^ Aapplication DB connection pool.
               -> Text
               -- ^ Text form of the payload tag.
               -> UploadSchedule
               -- ^ Schedule uploads will be run on.
               -> Schedule ()
               -- ^ An action in the 'Schedule' monad.
scheduleUpload pool typeText (UploadSchedule schedule destination) =
  case fromText typeText of
    Nothing -> return ()
    Just importType -> modify (Job schedule job:)
      where job = runLoggingT $ runSqlPool (doUpload importType destination) pool

main :: IO ()
main = do
  configPath <- lookupEnv "CATCHER_CONFIG"
  config <- either (fail . prettyPrintParseException) return =<<
    decodeFileEither (fromMaybe "config.yaml" configPath)
  app <- runLoggingT (mkApp config)
  let schedules = view configSchedules config
      pool = view appConnPool app
  _ <- execSchedule $ itraverse_ (scheduleUpload pool) schedules
  let wapp = logStdout (serveApp app)
  case view configHttpsPort config of
    Just port -> case view configCertificate config of
      Just certPath ->
        runTLS (tlsSettings certPath certPath) (setPort port defaultSettings) wapp
      Nothing -> error "Missing certificate path"
    Nothing -> run port wapp
      where port = fromMaybe 8080 . view configHttpPort $ config
              
