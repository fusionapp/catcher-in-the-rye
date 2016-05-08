{-# OPTIONS_HADDOCK prune #-}

-- | Persistent model definitions for persisted data.
module Models
  ( migrateAll
  , DataImportId
  , DataImport(..)
  ) where

import Database.Persist.TH
import ImportType (ImportType)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
DataImport
    type ImportType
    email String
    deriving Show
|]
