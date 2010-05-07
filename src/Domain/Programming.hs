-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  alex.gerdes@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Domain.Programming
   ( module Domain.Programming.AlphaRenaming
   , module Domain.Programming.Anonymise
   , module Domain.Programming.InlinePatternBindings
   , module Domain.Programming.Strategies
   , module Domain.Programming.Strategies99
   , module Domain.Programming.HeliumRules
   , module Domain.Programming.Helium
   , module Domain.Programming.Prog
   , module Domain.Programming.PreludeS
   , module Domain.Programming.EncodingExercises
   , module Domain.Programming.Utils
   , module Domain.Programming.Exercises
   , module Domain.Programming.Transformations
   ) where
   
import Domain.Programming.AlphaRenaming (alphaRenaming)
import Domain.Programming.Anonymise
import Domain.Programming.InlinePatternBindings (inlinePatternBindings)
import Domain.Programming.Strategies
import Domain.Programming.Strategies99
import Domain.Programming.HeliumRules
import Domain.Programming.Helium
import Domain.Programming.Prog
import Domain.Programming.PreludeS
import Domain.Programming.EncodingExercises
import Domain.Programming.Utils
import Domain.Programming.Exercises
import Domain.Programming.Transformations

-- for convenience
import Common.Apply
import Common.Context
import Common.Rewriting
import Common.Strategy
import Control.Monad (unless, fail)
import Data.Generics.Biplate
import Data.Map hiding (map)
import Data.Maybe
--import Data.List
