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
module Common.Rewriting 
   ( RewriteRule, smartGenerator, rewriteRule
   , RuleBuilder, rewriteM, RuleSpec((:~>)), ruleSpecTerm, showRewriteRule
   , Rewrite(..), Operator, useOperators
   , associativeOperator, Operators, collectWithOperator, buildWithOperator
   , equalWith, isOperator, constructor, difference, differenceMode
   , acOperator, normalizeWith, IsTerm(..), Different(..)
   ) where

import Common.Rewriting.Difference
import Common.Rewriting.Operator
import Common.Rewriting.RewriteRule
import Common.Rewriting.Term