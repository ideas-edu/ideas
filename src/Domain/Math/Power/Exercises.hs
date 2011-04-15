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
import Data.Maybe
import Domain.Math.Power.Examples
import Domain.Math.Expr hiding (isPower)
import Domain.Math.Numeric.Views
import Domain.Math.Power.Rules
import Domain.Math.Power.Strategies
import Domain.Math.Power.NormViews
import Domain.Math.Power.Views


-- | Exercises ----------------------------------------------------------------

powerExercise :: LabeledStrategy (Context Expr) -> Exercise Expr
powerExercise s = makeExercise 
   { status        = Provisional
   , parser        = parseExpr
   , navigation    = navigator                     
   , strategy      = s
   }

simplifyPowerExercise :: Exercise Expr
simplifyPowerExercise = (powerExercise simplifyPowerStrategy)
   { exerciseId   = describe "simplify expression (powers)" $ 
                       newId "algebra.manipulation.exponents.simplify"
   , isReady      = isPowerAdd
   , isSuitable   = (`belongsTo` normPowerMapView)
   , equivalence  = withoutContext (viewEquivalent normPowerMapView)
   , examples     = level Medium $ concat $ simplerPowers 
                           ++ powers1 ++ powers2 
                           ++ negExp1 ++ negExp2
                           ++ normPower1 ++ normPower2 ++ normPower3
   , ruleOrdering = ruleOrderingWithId $ map getId
                      [ root2power, subExponents, reciprocalVar, addExponents
                      , mulExponents, distributePower ]
   }

powerOfExercise :: Exercise Expr
powerOfExercise = (powerExercise powerOfStrategy)
   { exerciseId   = describe "write as a power of a" $ 
                       newId "algebra.manipulation.exponents.powerof"
   , isReady      = isSimplePower
   , isSuitable   = (`belongsTo` normPowerView)
   , equivalence  = withoutContext (viewEquivalent normPowerNonNegRatio)
   , examples     = level Medium $ concat $  powersOfA ++ powersOfX 
                           ++ brokenExp1' ++ brokenExp2 ++ brokenExp3 
                           ++ normPower5' ++ normPower6
   , ruleOrdering = ruleOrderingWithId $ map getId
                      [ root2power, addExponents, subExponents, mulExponents
                      ,  distributePower, reciprocalVar ]
   }

nonNegBrokenExpExercise :: Exercise Expr
nonNegBrokenExpExercise = (powerExercise nonNegBrokenExpStrategy)
   { exerciseId   = describe "write with a non-negative exponent" $ 
                       newId "algebra.manipulation.exponents.nonnegative"
   , isReady      = isPower plainNatView
   , isSuitable   = (`belongsTo` normPowerNonNegDouble)
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

calcPowerExercise :: Exercise Expr
calcPowerExercise = (powerExercise calcPowerStrategy)
   { exerciseId   = describe "simplify expression (powers)" $ 
                       newId "arithmetic.exponents"
   , isReady      = isPowerAdd
   , isSuitable   = (`belongsTo` normPowerMapView)
   , equivalence  = withoutContext (viewEquivalent normPowerMapView)
   , examples     = level Medium $ concat $ negExp3 ++ normPower3' ++ normPower4
   }


-- | Ready checks -------------------------------------------------------------

isSimplePower :: Expr -> Bool
isSimplePower (Sym s [Var _, y]) 
                 | isPowerSymbol s = y `belongsTo` rationalView
isSimplePower _ = False

isPower :: View Expr a -> Expr -> Bool
isPower v expr = 
  let Just (_, xs) = match productView expr 
      f (Nat 1 :/: a) = g a
      f a = g a
      g (Sym s [Var _, a]) | isPowerSymbol s = isJust (match v a)
      g (Sym s [x, Nat _]) | isRootSymbol s = isPower v x 
      g (Sqrt x) = g x
      g (Var _) = True
      g a = a `belongsTo` rationalView
  in distinct (concatMap vars xs) && all f xs
     
isPowerAdd :: Expr -> Bool
isPowerAdd expr =
  let Just xs = match sumView expr
  in all (isPower rationalView) xs && not (applicable calcPowerPlus expr)
