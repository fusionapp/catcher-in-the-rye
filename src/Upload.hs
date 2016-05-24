-- | Upload data to its final destination.
module Upload
  ( doUpload
  ) where

import Control.Exception (try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (traverse_)
import Data.Traversable (for)
import Database.Persist (selectList, deleteWhere, (==.), entityVal)
import Network.HTTP.Client (HttpException)
import Network.Wreq (post)

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
    ns <- liftIO . for destinations $ \destination -> do
      r <- try $ traverse_ (post destination . payloadData . entityVal) payloads
      return $ case r of
        Left (e :: HttpException) -> Failure tag destination (show e)
        Right _ -> Success tag destination
    deleteWhere [PayloadType ==. tag]
    return ns
  liftIO $ traverse_ (deliverNotifications notifications) targets
