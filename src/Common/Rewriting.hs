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
   ( match, matchM, RewriteRule, smartGenerator, rewriteRule, rewriteRules
   , Builder, rewriteM, RuleSpec((:~>)), rulePair, BuilderList
   , Rewrite(..), ShallowEq(..), MetaVar(..), Operator
   , associativeOperator, ruleName, Operators, collectWithOperator
   , equalWith, isOperator, constructor, difference, differenceMode
   , acOperator, normalizeWith, IsTerm(..), Different(..)
   {- module Common.Rewriting.AC
   , module Common.Rewriting.Confluence
   , module Common.Rewriting.MetaVar
   , module Common.Rewriting.RewriteRule
   , module Common.Rewriting.Substitution
   , module Common.Rewriting.Difference
   , module Common.Rewriting.Unification -}
   ) where

import Common.Rewriting.AC
import Common.Rewriting.Confluence ()
import Common.Rewriting.MetaVar
--import Common.Rewriting.RewriteRule 
--   ( RewriteRule, smartGenerator, rewriteRule, rewriteRules
--   , Builder, rewriteM, RuleSpec((:~>)), rulePair, ruleName, BuilderList)
import Common.Rewriting.Substitution ()
import Common.Rewriting.Difference
--import Common.Rewriting.Unification (Rewrite(..), ShallowEq(..))


import Common.Rewriting.Term hiding (match)

match = ()
matchM = ()