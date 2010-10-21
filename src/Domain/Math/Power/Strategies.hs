{-# OPTIONS -XNoMonomorphismRestriction #-}
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
module Domain.Math.Power.Strategies
   ( powerStrategy
   , powerOfStrategy
   , calcPowerStrategy
   , nonNegExpStrategy
   , powerEqStrategy
   , powerEqApproxStrategy
   , expEqStrategy
   , logEqStrategy
--   , higherPowerEqStrategy
   ) where

import Common.Classes
import Common.Context
import Common.Navigator
import Common.Rewriting.Term
import Common.Strategy
import Common.Transformation
import Common.View
import Data.Maybe
import Domain.Math.Data.Relation
import Domain.Math.Data.OrList
import Domain.Math.Expr
import Domain.Math.Equation.CoverUpRules
import Domain.Math.Polynomial.Strategies (quadraticStrategy, linearStrategy)
import Domain.Math.Polynomial.Rules (flipEquation)
import Domain.Math.Power.Rules
import Domain.Math.Numeric.Rules
import Domain.Math.Numeric.Views
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
      <*> option (use greatestPower <*> use commonPower)
      <*> (use nthRoot <|> use nthPower) 
    
    cleanup = applyD $ repeat $ alternatives $ map (somewhere . use) $ 
                fractionPlus : naturalRules ++ rationalRules


powerEqApproxStrategy :: LabeledStrategy (Context (Relation Expr))
powerEqApproxStrategy = label "Power equation with approximation" $
  powerEqStrategy <*> try (use approxPower)

-- higherPowerEqStrategy :: LabeledStrategy (Context (OrList (Relation Expr)))
-- higherPowerEqStrategy = label "Higher power equation" $ powerEqStrategy

expEqStrategy :: LabeledStrategy (Context (Equation Expr))
expEqStrategy = cleanUpStrategy cleanup strat
  where 
    strat =  label "Exponential equation" 
          $  try coverup
         <*> repeat (somewhereNotInExp (use factorAsPower))
         <*> powerS 
         <*> (use sameBase <|> use equalsOne)
         <*> linearStrategy
         
    somewhereNotInExp = somewhereWith "somewhere but not in exponent" $ \ a ->
      if (isPowC a) then [1] else [0 .. arity a-1]
    
    isPowC = maybe False (isJust . isPower :: Term -> Bool) . currentT
    
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

------------------------------------------------------------------------------

powerStrategy :: LabeledStrategy (Context Expr)
powerStrategy = makeStrategy "simplify" rules cleanupRules
  where 
    rules = powerRules 
    cleanupRules = calcPower : naturalRules ++ rationalRules

powerOfStrategy :: LabeledStrategy (Context Expr)
powerOfStrategy = makeStrategy "write as power of" rules cleanupRules
  where
   rules = powerRules 
   cleanupRules = calcPower 
                : simplifyRoot 
                : simplifyFraction 
                : naturalRules 
               ++ rationalRules

nonNegExpStrategy :: LabeledStrategy (Context Expr)
nonNegExpStrategy = cleanUpStrategy cleanup $ strategise "non neg exponent" rules
  where
    rules = [ addExponents
            , subExponents
            , mulExponents
            , reciprocalInv
            , distributePower
            , distributePowerDiv
            , power2root
            , distributeRoot
            , zeroPower
            , calcPowerPlus
            , calcPowerMinus
            , myFractionTimes
            , reciprocalFrac
            ] ++ fractionRules
    cleanup = applyD $ repeat $ alternatives $
                simp : (map (somewhere . liftToContext) $ calcPower : naturalRules)
    simp = (liftToContext simplifyFraction) <*> not (somewhere $ liftToContext myFractionTimes)
  
calcPowerStrategy :: LabeledStrategy (Context Expr)
calcPowerStrategy = makeStrategy "calcPower" rules cleanupRules
  where
    rules = calcPower 
          : mulRootCom
          : divRoot 
          : rationalRules
    cleanupRules = rationalRules ++ naturalRules

------------------------------------------------------------
-- | Help functions

makeStrategy :: String -> [Rule Expr] -> [Rule Expr] -> LabeledStrategy (Context Expr)
makeStrategy l rs cs = cleanUpStrategy f $ strategise l rs
  where
    f = applyD $ strategise l cs

strategise l = label l . repeat . alternatives . map (somewhere . liftToContext)
--    strategise l = label l . Common.Strategy.replicate 100 . try . alternatives . map (somewhere . liftToContext)

powerRules =
      [ addExponents
      , subExponents
      , mulExponents
      , distributePower
      , zeroPower
      , reciprocal
      , root2power
      , distributeRoot
      , calcPower
      , calcPowerPlus
      , calcPowerMinus
      , myFractionTimes
      , pushNegOut
      ]

-- | Allowed numeric rules
naturalRules =
   [ calcPlusWith     "nat" myNatView
   , calcMinusWith    "nat" myNatView
   , calcTimesWith    "nat" myNatView
   , calcDivisionWith "nat" myNatView
   , doubleNegate
   , negateZero
   , plusNegateLeft
   , plusNegateRight
--   , minusNegateLeft
   , minusNegateRight
   , timesNegateLeft
   , timesNegateRight   
   , divisionNegateLeft
   , divisionNegateRight
   ]

myNatView = makeView f fromInteger
  where
    f (Nat n) = Just n
    f _       = Nothing
 
rationalRules =    
   [ calcPlusWith     "rational" rationalRelaxedForm
   , calcMinusWith    "rational" rationalRelaxedForm
   , calcTimesWith    "rational" rationalRelaxedForm
   , calcDivisionWith "integer"  integerNormalForm
   , doubleNegate
   , negateZero
   , divisionDenominator
   , divisionNumerator
   , simplerFraction
   ]
   
fractionRules =
   [ fractionPlus, fractionPlusScale, fractionTimes
   , calcPlusWith     "integer" integerNormalForm
   , calcMinusWith    "integer" integerNormalForm
   , calcTimesWith    "integer" integerNormalForm -- not needed?
   , calcDivisionWith "integer" integerNormalForm
   , doubleNegate
   , negateZero
   , smartRule divisionDenominator  
   , smartRule divisionNumerator 
   , simplerFraction
   ]
