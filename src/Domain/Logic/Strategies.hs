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
module Domain.Logic.Strategies 
   ( dnfStrategy, dnfStrategyDWA) where

import Prelude hiding (repeat)
import Domain.Logic.Rules
import Domain.Logic.GeneralizedRules
import Domain.Logic.Formula
import Common.Context (Context, liftToContext, fromContext)
import Common.Rewriting (isOperator)
import Common.Transformation
import Common.Strategy

-----------------------------------------------------------------------------
-- To DNF, with priorities (the "DWA" approachs)

dnfStrategyDWA :: LabeledStrategy (Context SLogic)
dnfStrategyDWA =  label "Bring to dnf (DWA)" $ 
   repeat $ somewhereOr $  
      label "Simplify"                            simplify
      |> label "Eliminate implications/equivalences" eliminateImplEquiv
      |> label "Eliminate nots"                      eliminateNots
      |> label "Move ors to top"                     orToTop
 where
    simplify =  somewhere $ useRules
       [ ruleFalseZeroOr, ruleTrueZeroOr, ruleTrueZeroAnd
       , ruleFalseZeroAnd, ruleNotTrue, ruleNotFalse
       , ruleNotNot, ruleIdempOr, ruleIdempAnd, ruleAbsorpOr, ruleAbsorpAnd
       , ruleComplOr, ruleComplAnd
       ]
    eliminateImplEquiv = somewhere $ useRules
       [ ruleDefImpl, ruleDefEquiv
       ]
    eliminateNots = somewhere $ useRules
       [ generalRuleDeMorganAnd, generalRuleDeMorganOr
       , ruleDeMorganAnd, ruleDeMorganOr
       ]
    orToTop = somewhere $ useRules 
       [ generalRuleAndOverOr, ruleAndOverOr ]

-- A specialized variant of the somewhere traversal combinator. Apply 
-- the strategy only at (top-level) disjuncts 
somewhereOr :: IsStrategy f => f (Context SLogic) -> Strategy (Context SLogic)
somewhereOr s =
   let isOr = isOperator orOperator . fromContext
   in fix $ \this -> s <|> check isOr <*> once this

-----------------------------------------------------------------------------
-- To DNF, in four steps

dnfStrategy :: LabeledStrategy (Context SLogic)
dnfStrategy =  label "Bring to dnf"
      $  label "Eliminate constants"                 eliminateConstants
     <*> label "Eliminate implications/equivalences" eliminateImplEquiv
     <*> label "Eliminate nots"                      eliminateNots 
     <*> label "Move ors to top"                     orToTop
 where
   eliminateConstants = repeat $ topDown $ useRules
      [ ruleFalseZeroOr, ruleTrueZeroOr, ruleTrueZeroAnd
      , ruleFalseZeroAnd, ruleNotTrue, ruleNotFalse, ruleFalseInEquiv
      , ruleTrueInEquiv, ruleFalseInImpl, ruleTrueInImpl
      ]
   eliminateImplEquiv = repeat $ bottomUp $ useRules
      [ ruleDefImpl, ruleDefEquiv 
      ] 
   eliminateNots = repeat $ topDown $ 
      useRules
         [ generalRuleDeMorganAnd, generalRuleDeMorganOr ]
      |> useRules
         [ ruleDeMorganAnd, ruleDeMorganOr
         , ruleNotNot
         ]
   orToTop = repeat $ somewhere $  
      liftToContext generalRuleAndOverOr |> 
      liftToContext ruleAndOverOr
      
-- local helper function
useRules :: [Rule SLogic] -> Strategy (Context SLogic)
useRules = alternatives . map liftToContext