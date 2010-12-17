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
-----------------------------------------------------------------------------
module Domain.Logic
   ( module Domain.Logic.BuggyRules
   , module Domain.Logic.Exercises
   , module Domain.Logic.Formula
   , module Domain.Logic.GeneralizedRules
   , module Domain.Logic.Generator
   , module Domain.Logic.Parser
   , module Domain.Logic.Rules
   , module Domain.Logic.Strategies
   ) where

import Domain.Logic.BuggyRules hiding (rule, ruleList)
import Domain.Logic.Exercises
import Domain.Logic.Formula
import Domain.Logic.GeneralizedRules
import Domain.Logic.Generator
import Domain.Logic.Parser
import Domain.Logic.Rules      hiding (rule, ruleList)
import Domain.Logic.Strategies