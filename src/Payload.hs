-- | Payload handling.
module Payload
  ( storePayload
  ) where

import App (AppM, runDB)
import Control.Monad (void)
import Database.Persist (insert, deleteWhere, (==.))
import Models

-- | Store a payload in the database.
--
-- Any existing payloads with the same tag will be replaced.
storePayload :: Payload -> AppM ()
storePayload payload = void . runDB $ do
    deleteWhere [PayloadType ==. payloadType payload]
    insert payload
