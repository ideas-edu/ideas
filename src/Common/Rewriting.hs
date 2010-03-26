-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Common.Rewriting 
   ( RewriteRule, smartGenerator, rewriteRule, rewriteRules
   , Builder, rewriteM, RuleSpec((:~>)), rulePair, BuilderList, showRewriteRule
   , Rewrite(..), ShallowEq(..), Operator
   , associativeOperator, ruleName, Operators, collectWithOperator
   , equalWith, isOperator, constructor, difference, differenceMode
   , acOperator, normalizeWith, IsTerm(..), Different(..)
   ) where

import Common.Rewriting.AC
import Common.Rewriting.Difference
import Common.Rewriting.RewriteRule
import Common.Rewriting.Term