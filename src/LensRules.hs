-- | Rules for making lenses.
module LensRules
  ( noSigs
  ) where

import Control.Lens (LensRules, lensRules, generateSignatures, (.~))
import Data.Function ((&))

-- | Make lenses without signatures.
--
-- Writing the signatures out manually allows attaching Haddocks to them.
noSigs :: LensRules
noSigs = lensRules & generateSignatures .~ False
