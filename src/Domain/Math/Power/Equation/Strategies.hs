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
   -- ( powerEqStrategy
   -- , powerEqApproxStrategy
   -- , expEqStrategy
   -- , logEqStrategy
   -- , higherPowerEqStrategy
   -- ) 
   where

import Prelude hiding (repeat, not)

import Common.Classes
import Common.Context
import Common.Exercise
import Common.Id
import Common.Navigator
import Common.Rewriting
import Common.Strategy
import Data.Maybe
import Domain.Math.Data.Relation
import Domain.Math.Data.OrList
import Domain.Math.Expr
import Domain.Math.Equation.CoverUpRules
import Domain.Math.Polynomial.Strategies (quadraticStrategy, linearStrategy)
import Domain.Math.Polynomial.Rules (flipEquation)
import Domain.Math.Power.Rules
import Domain.Math.Power.Utils
import Domain.Math.Power.Equation.Rules
import Domain.Math.Numeric.Rules
import Domain.Math.Simplification


-- | Strategies ---------------------------------------------------------------

--powerEqStrategy :: (IsTerm a, Simplify a) => LabeledStrategy (Context a)
powerEqStrategy = cleanUpStrategy clean $ label "Power equation" $ repeat
   $  try myCoverUpStrategy
  <*> option (use greatestPower <*> use commonPower)
  <*> use nthRoot
  <*> remove (label "useApprox" $ try $ use approxPower)
  where    
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
          $  try myCoverUpStrategy
         <*> repeat (somewhereNotInExp (use factorAsPower))
         <*> powerS 
         <*> (use sameBase <|> use equalsOne)
         <*> linearStrategy
           
    cleanup = applyD $ exhaustiveUse $ {-  simplifyProduct : -} natRules ++ rationalRules

    natRules =
      [ calcPlusWith     "nat" plainNatView
      , calcMinusWith    "nat" plainNatView
      , calcTimesWith    "nat" plainNatView
      , calcDivisionWith "nat" plainNatView
      , doubleNegate
      , negateZero
      ]

    powerS = repeat $ somewhere $ alternatives 
      [ use root2power, use addExponents, use subExponents, use mulExponents
      ] -- , use reciprocal, use reciprocalFor ]

logEqStrategy :: LabeledStrategy (Context (OrList (Relation Expr)))
logEqStrategy = label "Logarithmic equation"
              $  use logarithm
             <*> try (use flipEquation)
             <*> repeat (somewhere $  use nthRoot 
                                  <|> use calcPower 
                                  <|> use calcPowerPlus 
                                  <|> use calcPowerMinus
                                  <|> use calcRoot
                                  <|> use calcPowerRatio)
             <*> quadraticStrategy


higherPowerEqStrategy :: LabeledStrategy (Context (OrList (Relation Expr)))
higherPowerEqStrategy =  cleanUpStrategy cleanup strat
  where 
    strat = label "Higher power equation" 
          $  succeed -- powerEqStrategy
         <*> try (somewhereNotInExp (use factorAsPower) <*> try (somewhere (use mulExponents)))
         
    cleanup = applyD $ repeat $ alternatives $ map (somewhere . use) $ 
                onePower : rationalRules


somewhereNotInExp = somewhereWith "somewhere but not in exponent" f
  where
    f a = if isPowC a then [1] else [0 .. arity a-1]
    isPowC = maybe False (isJust . isPower :: Term -> Bool) . currentT


-- | Help functions -----------------------------------------------------------

myCoverUpStrategy :: IsTerm a => Strategy (Context a)
myCoverUpStrategy = repeat $ alternatives $ map use coverUpRules
