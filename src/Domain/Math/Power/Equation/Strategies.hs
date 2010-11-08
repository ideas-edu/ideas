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
module Domain.Math.Power.Equation.Strategies
   ( powerEqStrategy
   , powerEqApproxStrategy
   , expEqStrategy
   , logEqStrategy
   , higherPowerEqStrategy
   ) where

import Common.Classes
import Common.Context
import Common.Navigator
import Common.Rewriting.Term
import Common.Strategy
import Common.Transformation
import Common.View
import Control.Monad
import Data.Maybe
import Domain.Math.Data.Relation
import Domain.Math.Data.OrList
import Domain.Math.Expr
import Domain.Math.Equation.CoverUpRules
import Domain.Math.Polynomial.Strategies (quadraticStrategy, linearStrategy)
import Domain.Math.Polynomial.Rules (flipEquation)
import Domain.Math.Power.Rules
import Domain.Math.Power.Views
import Domain.Math.Numeric.Rules
import Domain.Math.Numeric.Views
import Domain.Math.Equation.CoverUpExercise
import Prelude hiding (repeat, not)


------------------------------------------------------------
-- Strategies

logEqStrategy :: LabeledStrategy (Context (OrList (Relation Expr)))
logEqStrategy = label "Logarithmic equation"
              $  (use logarithm)
             <*> try (use flipEquation)
             <*> repeat (somewhere $  use nthRoot 
                                  <|> use calcPower 
                                  <|> use calcPowerPlus 
                                  <|> use calcPowerMinus
                                  <|> use calcRoot
                                  <|> use calcPowerRatio)
             <*> quadraticStrategy

powerEqStrategy :: IsTerm a => LabeledStrategy (Context a)
powerEqStrategy = cleanUpStrategy cleanup strat
  where 
    strat = label "Power equation" $ repeat $
          try (repeat $ alternatives $ map use coverUpRules)
--          try coverUpStrategy
      <*> option (use greatestPower <*> use commonPower)
      <*> (use nthRoot <|> use nthPower) 
    
    cleanup = applyD $ repeat $ alternatives $ map (somewhere . use) $ 
                onePower : fractionPlus : naturalRules ++ rationalRules

powerEqApproxStrategy :: LabeledStrategy (Context (Relation Expr))
powerEqApproxStrategy = label "Power equation with approximation" $
  powerEqStrategy <*> try (use approxPower)

higherPowerEqStrategy :: LabeledStrategy (Context (OrList (Relation Expr)))
higherPowerEqStrategy =  cleanUpStrategy cleanup strat
  where 
    strat = label "Higher power equation" 
          $  powerEqStrategy
         <*> try ((somewhereNotInExp (use factorAsPower)) <*> try (somewhere (use mulExponents)))
         
    cleanup = applyD $ repeat $ alternatives $ map (somewhere . use) $ 
                onePower : rationalRules

expEqStrategy :: LabeledStrategy (Context (Equation Expr))
expEqStrategy = cleanUpStrategy cleanup strat
  where 
    strat =  label "Exponential equation" 
          $  try coverup
         <*> repeat (somewhereNotInExp (use factorAsPower))
         <*> powerS 
         <*> (use sameBase <|> use equalsOne)
         <*> linearStrategy
           
    cleanup = applyD $ repeat $ alternatives $ map (somewhere . use) $ 
                simplifyProduct : natRules ++ rationalRules
    natRules =
      [ calcPlusWith     "nat" myNatView
      , calcMinusWith    "nat" myNatView
      , calcTimesWith    "nat" myNatView
      , calcDivisionWith "nat" myNatView
      , doubleNegate
      , negateZero
      ]

    coverup = repeat $ alternatives $ map use coverUpRules

    powerS = repeat $ somewhere $  use root2powerG
                               <|> use addExponentsG
                               <|> use subExponents
                               <|> use mulExponents
                               <|> use reciprocalG
                               <|> use reciprocalFor

somewhereNotInExp = somewhereWith "somewhere but not in exponent" f
  where
    f a = if (isPowC a) then [1] else [0 .. arity a-1]
    isPowC = maybe False (isJust . isPower :: Term -> Bool) . currentT
