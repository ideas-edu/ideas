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
-- 
-----------------------------------------------------------------------------

module Domain.Math.Power.Exercises    
   ( -- * Power exercises
     simplifyPowerExercise
   , powerOfExercise 
   , nonNegBrokenExpExercise
   , calcPowerExercise
   ) where

import Prelude hiding ( (^) )

import Common.Utils (distinct)
import Common.Library
import Domain.Math.Power.Examples
import Domain.Math.Expr hiding (isPower)
import Domain.Math.Numeric.Views
import Domain.Math.Power.Rules
import Domain.Math.Power.Strategies
import Domain.Math.Power.NormViews
import Domain.Math.Power.Views


-- Exercises

powerExercise :: Exercise Expr
powerExercise = makeExercise 
   { status        = Provisional
   , parser        = parseExpr
   , navigation    = navigator                     
   }

-- | Simplify an expression containing powers as far as possible. This
-- exercise supports the following DWO-applets:
--
--  * HAVO B, hoofdstuk 7, activiteit 1
--
--  * VWO A/C, hoofdstuk 5, activiteit 3 t/m 6
--
--  * VWO B, hoofdstuk 4, activiteit 8, 9, part of 10
simplifyPowerExercise :: Exercise Expr
simplifyPowerExercise = powerExercise 
   { exerciseId   = describe "simplify expression (powers)" $ 
                       newId "algebra.manipulation.exponents.simplify"
   , strategy     = simplifyPowerStrategy
   , ready        = predicate isPowerAdd
   , suitable     = predicateView normPowerMapView
   , equivalence  = withoutContext (viewEquivalent normPowerMapView)
   , examples     = level Medium $ concat $ 
                              simplerPowers 
                           ++ powers1 ++ powers2 
                           ++ negExp1 ++ negExp2
                           ++ normPower1 ++ normPower2 ++ normPower3
   , ruleOrdering = ruleOrderingWithId $ map getId
                      [ root2power, subExponents, reciprocalVar, addExponents
                      , mulExponents, distributePower ]
   }

-- | The @powerOfExercise@ is more strict than the 'simplifyPowerExercise'.
-- It only allows one variable experssions. This exercise supports the 
-- following DWO-applets:
--
--  * HAVO B, hoofdstuk 7, activiteit 2 and 4
--
--  * VWO A/C, hoofdstuk 5, activiteit part of 10 and 11 and 12
--
--  * VWO B, hoofdstuk 4, activiteit 12 partly, and 13
powerOfExercise :: Exercise Expr
powerOfExercise = powerExercise
   { exerciseId   = describe "write as a power of a" $ 
                       newId "algebra.manipulation.exponents.powerof"
   , ready        = predicate isSimplePower
   , strategy     = simplifyPowerStrategy
   , suitable     = predicateView normPowerView
   , equivalence  = withoutContext (viewEquivalent normPowerNonNegRatio)
   , examples     = level Medium $ concat $  powersOfA ++ powersOfX 
                           ++ brokenExp1' ++ brokenExp2 ++ brokenExp3 
                           ++ normPower5' ++ normPower6
   , ruleOrdering = ruleOrderingWithId $ map getId
                      [ root2power, addExponents, subExponents, mulExponents
                      ,  distributePower, reciprocalVar ]
   }

-- | Rewrite power expressions so that they have any negative or broken
-- exponents. Supported DWO-applets:
--
--  * HAVO B, hoofdstuk 7, activiteit 3 and 5
--
--  * VWO A/C, hoofdstuk 5, activiteit 8,9  and part of 10
--
--  * VWO B, hoofdstuk 4, activiteit 11 partly, and 12 partly
nonNegBrokenExpExercise :: Exercise Expr
nonNegBrokenExpExercise = powerExercise
   { exerciseId   = describe "write with a non-negative exponent" $ 
                       newId "algebra.manipulation.exponents.nonnegative"
   , strategy     = nonNegBrokenExpStrategy
   , ready        = predicate (isPower plainNatView)
   , suitable     = predicateView normPowerNonNegDouble
   , equivalence  = withoutContext (viewEquivalent normPowerNonNegDouble)
   , examples     = level Medium $ concat $  nonNegExp ++ nonNegExp2 ++ negExp4 ++ negExp5 
                           ++ brokenExp1 
                           ++ normPower4' ++ normPower5
   , ruleOrdering = ruleOrderingWithId [ getId mulExponents
                                       , getId reciprocalFrac
                                       , getId reciprocalInv
                                       , getId power2root
                                       , getId distributePower ]
   }

-- | Calculate the integer number for the given power expression. Supported
-- DWO-applets:
--
--  * VWO A/C, hoofdstuk 5, activiteit 7
--
--  * VWO B, hoofdstuk 4, activiteit 10 partly, 11 partly
calcPowerExercise :: Exercise Expr
calcPowerExercise = powerExercise
   { exerciseId   = describe "simplify expression (powers)" $ 
                       newId "arithmetic.exponents"
   , strategy     = calcPowerStrategy
   , ready        = predicate isPowerAdd
   , suitable     = predicateView normPowerMapView
   , equivalence  = withoutContext (viewEquivalent normPowerMapView)
   , examples     = level Medium $ concat $ negExp3 ++ normPower3' ++ normPower4
   }


-- Ready checks

isSimplePower :: Expr -> Bool
isSimplePower (Sym s [Var _, y]) 
                 | isPowerSymbol s = y `belongsTo` rationalView
isSimplePower _ = False

isPower :: View Expr a -> Expr -> Bool
isPower v expr = 
  let xs = snd (from productView expr)
      f (Nat 1 :/: a) = g a
      f a = g a
      g (Sym s [Var _, a]) | isPowerSymbol s = a `belongsTo` v
      g (Sym s [x, Nat _]) | isRootSymbol s = isPower v x 
      g (Sqrt x) = g x
      g (Var _) = True
      g a = a `belongsTo` rationalView
  in distinct (concatMap vars xs) && all f xs
     
isPowerAdd :: Expr -> Bool
isPowerAdd expr =
  let xs = from sumView expr
  in all (isPower rationalView) xs && not (applicable calcPowerPlus expr)
