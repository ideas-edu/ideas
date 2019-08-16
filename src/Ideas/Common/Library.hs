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
-- Exports most from package Common
--
-----------------------------------------------------------------------------

module Ideas.Common.Library
   ( module Export
   , failS, notS, repeatS, replicateS, sequenceS, untilS
   , Some(..)
   ) where

import Ideas.Common.Classes as Export
import Ideas.Common.Constraint as Export hiding (Result(..))
import Ideas.Common.Context as Export
import Ideas.Common.Derivation as Export
import Ideas.Common.Environment as Export
import Ideas.Common.Exercise as Export
import Ideas.Common.Id as Export
import Ideas.Common.Predicate as Export
import Ideas.Common.Rewriting as Export
import Ideas.Common.Rule as Export
import Ideas.Common.Strategy as Export hiding (fail, not, repeat, replicate, sequence, until)
import Ideas.Common.Traversal.Navigator as Export (Location, location, toLocation, fromLocation, arity, top)
import Ideas.Common.View as Export
import Ideas.Utils.Prelude as Export (readM, Some(..))

import qualified Ideas.Common.Strategy as S

-- | Alias for strategy combinator @fail@
failS :: Strategy a
failS = S.fail

-- | Alias for strategy combinator @not@
notS :: IsStrategy f => f a -> Strategy a
notS = S.not

-- | Alias for strategy combinator @repeat@
repeatS :: IsStrategy f => f a -> Strategy a
repeatS = S.repeat

-- | Alias for strategy combinator @replicate@
replicateS :: IsStrategy f => Int -> f a -> Strategy a
replicateS = S.replicate

-- | Alias for strategy combinator @sequence@
sequenceS :: IsStrategy f => [f a] -> Strategy a
sequenceS = S.sequence

-- | Alias for strategy combinator @until@
untilS :: IsStrategy f => (a -> Bool) -> f a -> Strategy a
untilS = S.until