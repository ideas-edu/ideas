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
   ( module Common.Classes, module Common.Transformation
   , module Common.Context, module Common.Navigator, module Common.Predicate
   , module Common.Derivation, module Common.DerivationTree, module Common.Id
   , module Common.Rewriting, module Common.Exercise
   , module Common.Strategy, module Common.View
   , failS, notS, repeatS, replicateS, sequenceS, untilS
   ) where

import Common.Classes
import Common.Context
import Common.Derivation
import Common.DerivationTree
import Common.Exercise
import Common.Id
import Common.Navigator hiding (left, right)
import Common.Predicate
import Common.Rewriting hiding (difference)
import Common.Strategy  hiding (fail, not, repeat, replicate, sequence, until)
import Common.Transformation
import Common.View

import qualified Common.Strategy as S
-- import Prelude (Bool, Int)

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