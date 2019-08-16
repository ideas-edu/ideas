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
-----------------------------------------------------------------------------

module Ideas.Common.Rewriting (module Export) where

import Ideas.Common.Rewriting.AutoTerm as Export (toTermG, fromTermG)
import Ideas.Common.Rewriting.RewriteRule as Export
import Ideas.Common.Rewriting.Term as Export