-- | Rules for making lenses.
module Rules
  ( noSigs
  , jsonOptions
  , slackOptions
  ) where

import Control.Lens (LensRules, lensRules, generateSignatures, set)
import Data.Aeson.TH (Options(..), SumEncoding(..), defaultOptions, defaultTaggedObject)
import Data.Aeson.Types (camelTo2)
import Data.Char (toLower)

-- | Make lenses without signatures.
--
-- Writing the signatures out manually allows attaching Haddocks to them.
noSigs :: LensRules
noSigs = set generateSignatures False lensRules

-- | Converts @prefixedCamelCase@ to @camel-case@.
modifier :: Char -> String -> String
modifier sep = drop 1 . dropWhile (/= sep) . camelTo2 sep

-- | Standard Aeson options for our configuration types.
jsonOptions :: Options
jsonOptions = 
  defaultOptions
  { fieldLabelModifier = modifier '-'
  , constructorTagModifier = map toLower
  , sumEncoding = defaultTaggedObject { tagFieldName = "type" }
  }

-- | Standard Aeson options for the Slack API types.
slackOptions :: Options
slackOptions =
  defaultOptions
  { fieldLabelModifier = modifier '_'
  , omitNothingFields = True
  }
