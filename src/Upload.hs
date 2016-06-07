-- | Upload data to its final destination.
module Upload
  ( doUpload
  ) where

import Control.Exception (try)
import Control.Lens (set)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (traverse_)
import Data.Traversable (for)
import Database.Persist (selectList, deleteWhere, (==.), entityVal)
import Network.HTTP.Client (HttpException, ManagerSettings(managerResponseTimeout))
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wreq (defaults, manager, postWith)
import Text.Show.Pretty (ppShow)

import App (AppT, runDB)
import Notifications (NotificationTarget, Notification(Success, Failure), deliverNotifications)
import Models
import PayloadTag (PayloadTag)

-- | Perform a scheduled upload run.
--
-- Payloads with the given tag will be POSTed to the given destination.
doUpload :: MonadIO m
         => PayloadTag
         -- ^ The type of payload to upload.
         -> [String]
         -- ^ The URI to upload it to.
         -> [NotificationTarget]
         -- ^ The targets to notify after the upload.
         -> AppT m ()
doUpload tag destinations targets = do
  notifications <- runDB $ do
    payloads <- selectList [PayloadType ==. tag] []
    ns <- liftIO $ uploadPayloads payloads
    unless (any isFailure ns) (deleteWhere [PayloadType ==. tag])
    return ns
  unless (null notifications) (liftIO $ traverse_ (deliverNotifications notifications) targets)
  where isFailure (Failure {}) = True
        isFailure _ = False
        uploadPayloads [] = return []
        uploadPayloads payloads = for destinations $ \destination -> do
          r <- try $ traverse_ (postWith options destination . payloadData . entityVal) payloads
          return $ case r of
            Left (e :: HttpException) -> Failure tag destination (ppShow e)
            Right _ -> Success tag destination
        options = set manager (Left (tlsManagerSettings {managerResponseTimeout = Just 600000000 })) defaults
