-----------------------------------------------------------------------------
-- Copyright 2019, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Generic terms
--
-----------------------------------------------------------------------------

module Ideas.Common.Rewriting.Term
   ( module Ideas.Common.Rewriting.Term.Data
   , module Ideas.Common.Rewriting.Term.Decoder
   , module Ideas.Common.Rewriting.Term.Class
   ) where

import Ideas.Common.Rewriting.Term.Data
import Ideas.Common.Rewriting.Term.Decoder hiding (Alternative(..))
import Ideas.Common.Rewriting.Term.Class