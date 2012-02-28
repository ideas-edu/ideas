-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  alex.gerdes@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------

module Domain.Math.Power.Equation.Strategies
   -- ( powerEqStrategy
   -- , powerEqApproxStrategy
   -- , expEqStrategy
   -- , logEqStrategy
   -- , higherPowerEqStrategy
   -- )
   where

import Common.Library
import Data.Maybe
import Domain.Math.CleanUp
import Domain.Math.Data.OrList
import Domain.Math.Data.Relation
import Domain.Math.Equation.CoverUpRules
import Domain.Math.Expr
import Domain.Math.Numeric.Rules
import Domain.Math.Polynomial.Rules (flipEquation, conditionVarsRHS)
import Domain.Math.Polynomial.Strategies (quadraticStrategy, linearStrategy)
import Domain.Math.Power.Equation.Rules
import Domain.Math.Power.Rules
import Domain.Math.Power.Utils

-- | Strategies ---------------------------------------------------------------

powerEqStrategy :: IsTerm a => LabeledStrategy (Context a)
powerEqStrategy = cleanUpStrategy clean strat
  where
    strat =  label "Power equation" $ repeatS
          $  myCoverUpStrategy
         <*> option (use greatestPower <*> use commonPower)
         <*> use nthRoot
         <*> remove (label "useApprox" $ try $ use approxPower)

    clean = applyD $ exhaustiveUse rules
    rules = onePower : fractionPlus : naturalRules ++ rationalRules

powerEqApproxStrategy :: LabeledStrategy (Context (Relation Expr))
powerEqApproxStrategy = label "Power equation with approximation" $
  configureNow (configure cfg powerEqStrategy)
    where
      cfg = [ (byName (newId "useApprox"), Reinsert) ]

expEqStrategy :: LabeledStrategy (Context (Equation Expr))
expEqStrategy = cleanUpStrategy cleanup strat
  where
    strat =  label "Exponential equation"
          $  myCoverUpStrategy
         <*> repeatS (somewhereNotInExp (use factorAsPower))
         <*> repeatS (somewhereNotInExp (use reciprocal))
         <*> powerS
         <*> (use sameBase <|> use equalsOne)
         <*> linearStrategy

    cleanup = applyD (exhaustiveUse $ naturalRules ++ rationalRules)
            . applyTop (fmap (mergeConstantsWith isIntRatio))

    isIntRatio x = x `belongsTo` myIntegerView || x `belongsTo` v
      where v = divView >>> first myIntegerView >>> second myIntegerView

    powerS = exhaustiveUse [ root2power, addExponents, subExponents
                           , mulExponents,  simpleAddExponents ]

logEqStrategy :: LabeledStrategy (Context (OrList (Relation Expr)))
logEqStrategy = label "Logarithmic equation"
              $  try (use logarithm)
             <*> try (use (check conditionVarsRHS) <*> use flipEquation)
             <*> repeatS (somewhere $  use nthRoot
                                   <|> use calcPower
                                    <|> use calcPowerPlus
                                   <|> use calcPowerMinus
                                   <|> use calcPlainRoot
                                   <|> use calcPowerRatio)
             <*> quadraticStrategy

higherPowerEqStrategy :: LabeledStrategy (Context (OrList (Equation Expr)))
higherPowerEqStrategy =  cleanUpStrategy cleanup coverUpStrategy'
  where
    cleanup = applyTop $ fmap $ fmap cleanUpExpr

rootEqStrategy :: LabeledStrategy (Context (OrList (Equation Expr)))
rootEqStrategy =  cleanUpStrategy cleanup strat
  where
    strat =  label "Cover up"
          $ try ( use (check condXisRight) <*> use flipEquation )
         <*> exhaustiveSomewhere myCoverUpRulesOr
    cleanup = applyTop $ fmap $ fmap cleanUpExpr

-- | Help functions -----------------------------------------------------------

myCoverUpStrategy :: IsTerm a => Strategy (Context a)
myCoverUpStrategy = repeatS $ alternatives $ map use coverUpRules

coverUpStrategy' :: LabeledStrategy (Context (OrList (Equation Expr)))
coverUpStrategy' = cleanUpStrategy (applyTop $ fmap $ fmap cleanUpExpr) $
   label "Cover-up" $
   repeatS $ somewhere $ alternatives $ use coverUpRoot : coverUpRulesOr

somewhereNotInExp :: IsStrategy f => f (Context a) -> Strategy (Context a)
somewhereNotInExp = traverse [parentFilter f]
  where
    f a = if isPowC a then [0] else [0 .. arity a-1]
    isPowC = maybe False (isJust . isPower) . currentTerm

myConfigCoverUp :: ConfigCoverUp
myConfigCoverUp = configCoverUp
   { configName        = ""
   , predicateCovered  = elem "x" . vars
   , predicateCombined = notElem "x" . vars
   , coverLHS          = True
   , coverRHS          = True
   }

myCoverUpRulesOr :: IsTerm a => [Rule (Context a)]
myCoverUpRulesOr = use (coverUpPowerWith myConfigCoverUp)
                 : map (\f -> use $ f myConfigCoverUp) coverUpRulesWith

coverUpRulesWith :: [ConfigCoverUp -> Rule (Equation Expr)]
coverUpRulesWith =
   [ coverUpPlusWith, coverUpMinusLeftWith, coverUpMinusRightWith
   , coverUpNegateWith, {-myCoverUpTimesWith-} coverUpTimesWith, coverUpNumeratorWith
   , coverUpDenominatorWith, coverUpSqrtWith, coverUpRootWith
   ]