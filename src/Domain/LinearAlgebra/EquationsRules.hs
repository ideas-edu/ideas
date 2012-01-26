-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Domain.LinearAlgebra.EquationsRules 
   ( ruleCoverAllEquations, ruleUncoverEquation, ruleScaleEquation
   , ruleBackSubstitution, ruleIdentifyFreeVariables, ruleExchangeEquations
   , ruleEliminateVar, ruleCoverUpEquation, ruleInconsistentSystem
   , ruleDropEquation, equationsRules
   , simplifySystem, remaining, linId
   ) where

import Common.Library hiding (simplify)
import Control.Monad
import Data.List
import Data.Maybe
import Domain.LinearAlgebra.LinearSystem
import Domain.LinearAlgebra.LinearView
import Domain.LinearAlgebra.MatrixRules (covered, getCovered) -- for context
import Common.Utils
import Domain.Math.Data.Relation
import Domain.Math.Expr
import Domain.Math.Simplification (simplify)
import Prelude

linId :: Id
linId = newId "linearalgebra.linsystem"

equationsRules :: [Rule (Context (LinearSystem Expr))]
equationsRules =
   [ ruleExchangeEquations, ruleEliminateVar, ruleDropEquation
   , ruleInconsistentSystem
   , ruleScaleEquation, ruleBackSubstitution, ruleIdentifyFreeVariables
   , ruleCoverUpEquation, ruleUncoverEquation, ruleCoverAllEquations
   ]

ruleExchangeEquations :: Rule (Context (LinearSystem Expr))
ruleExchangeEquations = describe "Exchange two equations" $
   simplifySystem $ makeRule (linId, "exchange") $
   supplyContextParameters exchangeEquations $ \ls -> do
      mv  <- minvar ls
      eqs <- remaining ls
      i   <- findIndexM (elem mv . getVarsSystem . return) eqs
      cov <- getCovered
      return (cov, cov + i)

ruleEliminateVar :: Rule (Context (LinearSystem Expr))
ruleEliminateVar = describe "Eliminate a variable (using addition)" $
   simplifySystem $ makeRule (linId, "eliminate") $
   supplyContextParameters addEquations $ \ls -> do
      mv <- minvar ls
      hd:rest <- remaining ls
      let getCoef = coefficientOf mv . leftHandSide
      (i, coef) <- msum [ return (i, c) | (i, eq) <- zip [0..] rest, let c = getCoef eq, c /= 0 ]
      guard (getCoef hd /= 0)
      let v = negate coef / getCoef hd
      cov <- getCovered
      return (i + cov + 1, cov, v)

ruleDropEquation :: Rule (Context (LinearSystem Expr))
ruleDropEquation = describe "Drop trivial equations (such as 0=0)" $
   simplifySystem $ makeRule (linId, "trivial") $ makeTransEnv $ \ls -> do
      i   <- findIndexM (fromMaybe False . testConstants (==)) ls
      cov <- getCovered
      let f n = if i < n then n-1 else n
      covered := f cov
      return (deleteIndex i ls)

ruleInconsistentSystem :: Rule (Context (LinearSystem Expr))
ruleInconsistentSystem = describe "Inconsistent system (0=1)" $
   simplifySystem $ makeRule (linId, "inconsistent") $ makeTransEnv $ \ls -> do
      let stop = [0 :==: 1]
      guard (invalidSystem ls && ls /= stop)
      covered := 1 
      return stop

ruleScaleEquation :: Rule (Context (LinearSystem Expr))
ruleScaleEquation = describe "Scale equation to one" $
   simplifySystem $ makeRule (linId, "scale") $
   supplyContextParameters scaleEquation $ \ls -> do
      cov <- getCovered
      eq  <- elementAt cov ls
      let expr = leftHandSide eq
      mv <- minvar ls
      guard (coefficientOf mv expr /= 0)
      let coef = 1 / coefficientOf mv expr
      return (cov, coef)

ruleBackSubstitution :: Rule (Context (LinearSystem Expr))
ruleBackSubstitution = describe "Back substitution" $
   simplifySystem $ makeRule (linId, "subst") $
   supplyContextParameters addEquations $ \ls -> do
      cov <- getCovered
      eq  <- elementAt cov ls
      let expr = leftHandSide eq
      mv <- headM (vars expr)
      i  <- findIndexM ((/= 0) . coefficientOf mv . leftHandSide) (take cov ls)
      let coef = negate $ coefficientOf mv (leftHandSide (ls !! i))
      return (i, cov, coef)

ruleIdentifyFreeVariables :: IsLinear a => Rule (Context (LinearSystem a))
ruleIdentifyFreeVariables = describe "Identify free variables" $
   minorRule $ liftToContext $ makeSimpleRule (linId, "freevars") $ \ls ->
      let vs = [ head ys | ys <- map (vars . leftHandSide) ls, not (null ys) ]
          f eq =
             let (e1, e2) = splitLinearExpr (`notElem` vs) (leftHandSide eq) -- constant ends up in e1
             in e2 :==: rightHandSide eq - e1
      in Just (map f ls)

ruleCoverUpEquation :: Rule (Context (LinearSystem a))
ruleCoverUpEquation = describe "Cover up first equation" $
   minorRule $ makeRule (linId, "coverup") $ changeCover succ

ruleUncoverEquation :: Rule (Context (LinearSystem a))
ruleUncoverEquation = describe "Uncover one equation" $
   minorRule $ makeRule (linId, "uncover") $ changeCover pred

ruleCoverAllEquations :: Rule (Context (LinearSystem a))
ruleCoverAllEquations = describe "Cove all equations" $
   minorRule $ makeSimpleRule (linId, "coverall") $ \cls -> do
      ls <- fromContext cls
      return (insertRef covered (length ls) cls)

-- local helper functions
deleteIndex :: Int -> [a] -> [a]
deleteIndex i xs = ys ++ drop 1 zs
 where (ys, zs) = splitAt i xs

testConstants :: IsLinear a => (a -> a -> Bool) -> Equation a -> Maybe Bool
testConstants f (lhs :==: rhs)
   | hasNoVar lhs && hasNoVar rhs = Just (f lhs rhs)
   | otherwise = Nothing

-- simplify a linear system
simplifySystem :: Rule (Context (LinearSystem Expr)) -> Rule (Context (LinearSystem Expr))
simplifySystem = doAfter $ change (map (fmap f))
 where f = simplifyWith (fmap simplify) linearView

---------------------------------------------------------------------------------
-- Parameterized transformations

exchangeEquations :: ParamTrans (Int, Int) (LinearSystem a)
exchangeEquations = parameter2 "equation 1" "equation 2" exchange
 where
   exchange i j 
      | i > j     = exchange j i
      | otherwise = transMaybe $ \xs -> do
           guard (i/=j && validEquation i xs && validEquation j xs)
           let (begin, x:rest) = splitAt i xs
               (middle, y:end) = splitAt (j-i-1) rest
           return $ begin++[y]++middle++[x]++end

scaleEquation :: (Reference a, IsLinear a) => ParamTrans (Int, a) (LinearSystem a)
scaleEquation = parameter2 "equation" "scale factor" $ \i a -> transMaybe $ \xs -> do
   guard (a `notElem` [0,1])
   changeAt i (fmap (a*)) xs

addEquations :: (Reference a, IsLinear a) => ParamTrans (Int, Int, a) (LinearSystem a)
addEquations = parameter3 "equation 1" "equation 2" "scale factor" $ \i j a -> transMaybe $ \xs -> do
   guard (i/=j)
   j1 :==: j2 <- liftM (fmap (a*)) (elementAt j xs)
   let f (i1 :==: i2) = i1+j1 :==: i2+j2
   changeAt i f xs

changeCover :: (Int -> Int) -> Transformation (Context (LinearSystem a))
changeCover f = makeTransEnv_ $ \ls -> do
   new <- liftM f getCovered
   guard (new >= 0 && new <= length ls)
   covered := new

-- local helper function
validEquation :: Int -> [a] -> Bool
validEquation n xs = n >= 0 && n < length xs

-- | The equations that remain to be solved
remaining :: LinearSystem a -> EnvMonad (Equations a)
remaining ls = do
   cov <- getCovered
   return (drop cov ls)

-- | The minimal variable in the remaining equations
minvar :: IsLinear a => LinearSystem a -> EnvMonad String
minvar ls = do
   list <- liftM getVarsSystem (remaining ls)
   guard (not $ null list)
   return (minimum list)