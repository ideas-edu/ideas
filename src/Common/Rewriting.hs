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
   , Rewrite(..), useOperators
   , difference, differenceMode
   , IsTerm(..), Different(..)
   , module Common.Rewriting.Group
   ) where

import Common.Rewriting.Difference
import Common.Rewriting.Group hiding (identity)
import Common.Rewriting.RewriteRule
import Common.Rewriting.Term