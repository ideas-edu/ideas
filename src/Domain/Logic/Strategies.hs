-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (...add description...)
--
-----------------------------------------------------------------------------
module Domain.Logic.Strategies (toDNF, toDNF_DWA) where

import Prelude hiding (repeat)
import Domain.Logic.Rules
import Domain.Logic.Formula
import Common.Context (Context, liftRuleToContext)
import Common.Transformation
import Common.Strategy

-----------------------------------------------------------------------------
-- To DNF, in four steps

toDNF :: LabeledStrategy (Context Logic)
toDNF =  label "Bring to dnf"
      $  label "Eliminate constants"                 eliminateConstants
     <*> label "Eliminate implications/equivalences" eliminateImplEquiv
     <*> label "Eliminate nots"                      eliminateNots 
     <*> label "Move ors to top"                     orToTop
 where
   eliminateConstants = repeat $ topDown $ useRules
      [ ruleFalseZeroOr, ruleTrueZeroOr, ruleTrueZeroAnd
      , ruleFalseZeroAnd, ruleNotBoolConst, ruleFalseInEquiv
      , ruleTrueInEquiv, ruleFalseInImpl, ruleTrueInImpl
      ]
   eliminateImplEquiv = repeat $ bottomUp $ useRules
      [ ruleDefImpl, ruleDefEquiv 
      ] 
   eliminateNots = repeat $ topDown $ useRules
      [ ruleDeMorganAnd, ruleDeMorganOr, ruleNotNot
      ]
   orToTop = repeat $ somewhere $ liftRuleToContext 
      ruleAndOverOr

-----------------------------------------------------------------------------
-- To DNF, with priorities (the "DWA" approachs)

toDNF_DWA :: LabeledStrategy (Context Logic)
toDNF_DWA =  label "Bring to dnf (DWA)" $ 
   repeat $  label "Simplify"                            simplify
          |> label "Eliminate implications/equivalences" eliminateImplEquiv
          |> label "Eliminate nots"                      eliminateNots
          |> label "Move ors to top"                     orToTop
 where
    simplify =  somewhere $ useRules
       [ ruleFalseZeroOr, ruleTrueZeroOr, ruleTrueZeroAnd
       , ruleFalseZeroAnd, ruleNotBoolConst
       , ruleNotNot, ruleIdempOr, ruleIdempAnd, ruleAbsorpOr, ruleAbsorpAnd
       ]
    eliminateImplEquiv = somewhere $ useRules
       [ ruleDefImpl, ruleDefEquiv
       ]
    eliminateNots = somewhere $ useRules
       [ ruleDeMorganAnd, ruleDeMorganOr
       ]
    orToTop = somewhere $ liftRuleToContext 
       ruleAndOverOr
      
-- local helper function
useRules :: [Rule Logic] -> Strategy (Context Logic)
useRules = alternatives . map liftRuleToContext