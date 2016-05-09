-- | Upload data to its final destination.
module Upload
  ( doUpload
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logInfo)
import Data.Foldable (traverse_)
import Data.Monoid ((<>))
import Data.Text (pack)
import Database.Persist (selectList, deleteWhere, (==.), entityVal)
import Database.Persist.Sqlite (SqlPersistT)
import Models
import Network.Wreq (post)
import PayloadTag (PayloadTag)

-- | Perform a scheduled upload run.
--
-- Payloads with the given tag will be POSTed to the given destination.
doUpload :: (MonadIO m, MonadLogger m)
         => PayloadTag
         -- ^ The type of payload to upload.
         -> String
         -- ^ The URI to upload it to.
         -> SqlPersistT m ()
doUpload tag destination = do
  $(logInfo) $ "Performing scheduled upload to: " <> pack destination
  d <- selectList [PayloadType ==. tag] []
  liftIO $ traverse_ (post destination . payloadData . entityVal) d
  deleteWhere [PayloadType ==. tag]
