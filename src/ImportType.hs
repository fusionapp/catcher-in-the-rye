{-|
Module      : ImportType
Description : Enumeration type for data imports.

The main reason this is a separate module is TH stage restrictions.
-}
module ImportType
  ( asText
  , ImportType(..)
  ) where

import Data.Text (Text)
import Database.Persist.TH

-- | An enumeration type for the different types of data that can be imported.
data ImportType = ImportVehicle
                -- ^ Vehicle data.
  deriving (Show, Read, Eq)

-- | Obtain a Text identifier for an import type.
-- This is used as a key in the configuration.
asText :: ImportType -> Text
asText ImportVehicle = "vehicle"

derivePersistField "ImportType"
