-- | Tag type for payloads.
-- 
-- The main reason this is a separate module is TH stage restrictions.
module PayloadTag
  ( fromText
  , PayloadTag(..)
  ) where

import Data.Text (Text)
import Database.Persist.TH

-- | A tag type for the different types of data payloads that can be stored.
data PayloadTag = Vehicle
                | Motorite
  deriving (Show, Read, Eq)
derivePersistField "PayloadTag"

-- | Translate to/from a Text identifier for an import type.
--
-- This is used as a key in the configuration.
fromText :: Text -> Maybe PayloadTag
fromText v | v == "vehicle" = Just Vehicle
           | v == "motorite" = Just Motorite
           | otherwise = Nothing
