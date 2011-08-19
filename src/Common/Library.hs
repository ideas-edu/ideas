-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Exports most from package Common
--
-----------------------------------------------------------------------------
module Common.Library 
   ( module Export
   , failS, notS, repeatS, replicateS, sequenceS, untilS
   ) where

import Common.Classes as Export
import Common.Context as Export
import Common.Derivation as Export
import Common.DerivationTree as Export
import Common.Exercise as Export
import Common.Id as Export
import Common.Navigator as Export hiding (left, right)
import Common.Predicate as Export
import Common.Rewriting as Export
import Common.Strategy       as Export hiding (fail, not, repeat, replicate, sequence, until)
import Common.Transformation as Export
import Common.View as Export

import qualified Common.Strategy as S

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