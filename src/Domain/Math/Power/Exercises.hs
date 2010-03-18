-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
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
   ) where

import qualified Prelude
import Prelude hiding ( (^) )

import Common.Utils (distinct)
import Common.Apply 
import Common.Exercise
import Common.Strategy
import Common.View
import Common.Context
import Common.Navigator
import Common.Uniplate
import Common.Transformation
import Common.Derivation (derivations, derivation)
import Domain.Math.Examples.DWO3
import Domain.Math.Expr
import Domain.Math.Expr.Parser
import Domain.Math.Numeric.Views
import Domain.Math.Numeric.Rules
import Domain.Math.Numeric.Strategies
import Domain.Math.Power.Strategies
import Domain.Math.Power.Views
import Domain.Math.Power.Rules
import Domain.Math.Power.Tests

import Control.Monad
import Data.Maybe
import qualified Data.Map as M
--import Domain.Math.Power.Generators

------------------------------------------------------------
-- Exercises

isPower :: Expr -> Bool
isPower (Sym s [Var _,y]) | s==powerSymbol = y `belongsTo` rationalView
isPower _ = False

isPowerNonNeg :: Expr -> Bool
isPowerNonNeg expr = 
     let Just (_, xs) = match productView expr 
         f (Nat 1 :/: a) = g a
         f a = g a
         g (Sym s [Var _,Nat _]) | s==powerSymbol = True
         g (Sym s [x, Nat _]) | s==rootSymbol = g x
         g (Sqrt x) = g x
         g (Var _) = True
         g a = a `belongsTo` rationalView
     in distinct (concatMap collectVars xs) && all f xs

normPowerNonNeg :: View Expr (Rational, M.Map String Rational)
normPowerNonNeg = makeView f g
 where
     f expr = 
        case expr of
           Sym s [a,b] 
              | s==powerSymbol -> do
                   (r, m) <- f a
                   if r==1 
                     then do
                       r2 <- match rationalView b
                       return (1, M.map (*r2) m)
                     else do
                       n <- match integerView b
                       if n >=0 
                         then return (r Prelude.^ n, M.map (*fromIntegral n) m)
                         else return (1/(r Prelude.^ abs n), M.map (*fromIntegral n) m)
              | s==rootSymbol ->
                  f (Sym powerSymbol [a, 1/b])
           Sqrt a -> 
              f (Sym rootSymbol [a,2])
           a :*: b -> do
             (r1, m1) <- f a
             (r2, m2) <- f b
             return (r1*r2, M.unionWith (+) m1 m2)
           a :/: b -> do
             (r1, m1) <- f a
             (r2, m2) <- f b
             guard (r2 /= 0)
             return (r1/r2, M.unionWith (+) m1 (M.map negate m2))
           Var s -> return (1, M.singleton s 1)
           _ -> do
             r <- match rationalView expr
             return (fromRational r, M.empty)
     g (r, m) = 
       let xs = map f (M.toList m)
           f (s, r) = Var s .^. fromRational r
       in build productView (False, fromRational r : xs)

normPowerView :: View Expr (String, Rational)
normPowerView = makeView f g
 where
   f expr = 
        case expr of
           Sym s [x,y] 
              | s==powerSymbol -> do
                   (s, r) <- f x
                   r2 <- match rationalView y
                   return (s, r*r2)
              | s==rootSymbol -> 
                   f (x^(1/y))
           Sqrt x ->
              f (Sym rootSymbol [x, 2])
           Var s -> return (s, 1) 
           x :*: y -> do
             (s1, r1) <- f x
             (s2, r2) <- f y
             guard (s1==s2)
             return (s1, r1+r2)
           Nat 1 :/: y -> do
             (s, r) <- f y
             return (s, -r)
           x :/: y -> do
             (s1, r1) <- f x
             (s2, r2) <- f y
             guard (s1==s2)
             return (s1, r1-r2) 
           _ -> Nothing
             
   g (s, r) = Var s .^. fromRational r


powerExercise :: LabeledStrategy (Context Expr) -> Exercise Expr
powerExercise s = makeExercise 
   { status        = Provisional
   , parser        = parseExpr
   , navigation    = navigator                     
--   , equivalence   = viewEquivalent rationalView
   , strategy      = s
   }

simplifyPowerExercise :: Exercise Expr
simplifyPowerExercise = (powerExercise powerStrategy)
   { description  = "simplify expression (powers)"
   , exerciseCode = makeCode "math" "simplifyPower"
   , isReady      = isPowerNonNeg
   , examples     = concat $ simplerPowers ++ powers1 ++ powers2
   }

powerOfExercise :: Exercise Expr
powerOfExercise = (powerExercise powerStrategy)
   { description  = "write as a power of a"
   , exerciseCode = makeCode "math" "powerOf"
   , isReady      = isPower
   , isSuitable   = (`belongsTo` normPowerView)
   , equivalence  = viewEquivalent normPowerView
   , examples     = concat $ powersOfA ++ powersOfX
   }

nonNegExpExercise :: Exercise Expr
nonNegExpExercise = (powerExercise nonNegExpStrategy)
   { description  = "write with a non-negative exponent"
   , exerciseCode = makeCode "math" "nonNegExp"
   , isReady      = isPowerNonNeg
   , isSuitable   = (`belongsTo` normPowerNonNeg)
   , equivalence  = viewEquivalent normPowerNonNeg
   , examples     = concat $ nonNegExp ++ nonNegExp2
   }

-- | test stuff
showDerivations ex exercises level = 
  mapM_ (putStrLn . showDerivation ex) $ exercises !! level
                        
a = Var "a"
b = Var "b"
