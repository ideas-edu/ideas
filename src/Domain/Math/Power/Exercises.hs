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
   ( simplifyPowerExercise
   , powerOfExercise 
   , nonNegExpExercise
   , calcPowerExercise
   ) where

import Common.Classes 
import Common.Context
import Common.Exercise
import Common.Navigator
import Common.Strategy hiding (not, replicate)
import Common.Utils (distinct)
import Common.View
import Data.Maybe
import Domain.Math.Examples.DWO3
import Domain.Math.Expr
import Domain.Math.Numeric.Views
import Domain.Math.Power.Rules
import Domain.Math.Power.Strategies
import Domain.Math.Power.Views
import Prelude hiding ( (^) )

------------------------------------------------------------
-- Exercises

powerExercise :: LabeledStrategy (Context Expr) -> Exercise Expr
powerExercise s = makeExercise 
   { status        = Provisional
   , parser        = parseExpr
   , navigation    = navigator                     
   , strategy      = s
   }

simplifyPowerExercise :: Exercise Expr
simplifyPowerExercise = (powerExercise powerStrategy)
   { exerciseId   = describe "simplify expression (powers)" $ 
                       newId "algebra.manipulation.exponents.simplify"
   , isReady      = isPowerAdd
   , isSuitable   = (`belongsTo` normPowerView')
   , equivalence  = viewEquivalent normPowerView'
   , examples     = concat $  simplerPowers ++ powers1 ++ powers2 
                           ++ negExp1 ++ negExp2
                           ++ normPower1 ++ normPower2 ++ normPower3
   , ruleOrdering = ruleOrderingWithId powerRuleOrder                  
   }

powerOfExercise :: Exercise Expr
powerOfExercise = (powerExercise powerOfStrategy)
   { exerciseId   = describe "write as a power of a" $ 
                       newId "algebra.manipulation.exponents.powerof"
   , isReady      = isSimplePower
   , isSuitable   = (`belongsTo` normPowerView)
   , equivalence  = viewEquivalent normPowerNonNegRatio
   , examples     = concat $  powersOfA ++ powersOfX ++ brokenExp1' 
                           ++ brokenExp2 ++ brokenExp3 ++ normPower5'
                           ++ normPower6
   , ruleOrdering = ruleOrderingWithId powerRuleOrder             
   }

nonNegExpExercise :: Exercise Expr
nonNegExpExercise = (powerExercise nonNegExpStrategy)
   { exerciseId   = describe "write with a non-negative exponent" $ 
                       newId "algebra.manipulation.exponents.nonnegative"
   , isReady      = isPower natView
   , isSuitable   = (`belongsTo` normPowerNonNegDouble)
   , equivalence  = viewEquivalent normPowerNonNegDouble
   , examples     = concat $  nonNegExp ++ nonNegExp2 ++ negExp4 ++ negExp5 
                           ++ brokenExp1 ++ normPower4' ++ normPower5
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
   , isSuitable   = (`belongsTo` normPowerView')
   , equivalence  = viewEquivalent normPowerView'
   , examples     = concat $ negExp3 ++ normPower3' ++ normPower4
   }


----------------------------------------------------------------------
-- Ready checks

isSimplePower :: Expr -> Bool
isSimplePower (Sym s [Var _,y]) | s==powerSymbol = y `belongsTo` rationalView
isSimplePower _ = False

isPower :: View Expr a -> Expr -> Bool
isPower v expr = 
     let Just (_, xs) = match productView expr 
         f (Nat 1 :/: a) = g a
         f a = g a
         g (Sym s [Var _, a]) | s==powerSymbol = isJust (match v a)
         g (Sym s [x, Nat _]) | s==rootSymbol = isPower v x 
         g (Sqrt x) = g x
         g (Var _) = True
         g a = a `belongsTo` rationalView
     in distinct (concatMap collectVars xs) && all f xs
     
isPowerAdd :: Expr -> Bool
isPowerAdd expr =
  let Just xs = match sumView expr
  in all (isPower rationalView) xs && not (applicable calcPowerPlus expr)

-- test stuff
{-
showDerivations ex = mapM_ (putStrLn . showDerivation ex)

showAllDerivations ex = 
  mapM_ (\es -> putStrLn (replicate 80 '-') >> showDerivations ex es)
-}                        
a = Var "a"
b = Var "b"

