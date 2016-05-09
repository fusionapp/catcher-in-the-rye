{-# OPTIONS_HADDOCK prune #-}

-- | Persistent model definitions for persisted data.
module Models where

import Data.ByteString (ByteString)
import Database.Persist.Quasi
import Database.Persist.TH
import PayloadTag (PayloadTag)

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
  $(persistFileWith lowerCaseSettings "config/models")
