-- | Rules for making lenses.
module LensRules
  ( noSigs
  ) where

import Control.Lens (LensRules, lensRules, generateSignatures, set)

-- | Make lenses without signatures.
--
-- Writing the signatures out manually allows attaching Haddocks to them.
noSigs :: LensRules
noSigs = set generateSignatures False lensRules
