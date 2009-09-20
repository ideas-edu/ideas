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
module Domain.LinearAlgebra.EquationsRules where

import Prelude hiding (repeat)
import Common.Context
import Common.Transformation
import Common.Utils
import Control.Monad
import Data.List hiding (repeat)
import Data.Maybe
import Domain.Math.Data.Equation
import Domain.LinearAlgebra.LinearView
import Domain.LinearAlgebra.LinearSystem
import Domain.LinearAlgebra.MatrixRules (covered) -- for context
import Test.QuickCheck -- hopefully, temporarily

equationsRules :: (Argument a, IsLinear a) => [Rule (Context (LinearSystem a))]
equationsRules = [ ruleExchangeEquations, ruleEliminateVar, ruleDropEquation, ruleInconsistentSystem
                 , ruleScaleEquation, ruleBackSubstitution, ruleIdentifyFreeVariables
                 , ruleCoverUpEquation, ruleUncoverEquation, ruleCoverAllEquations ]

ruleExchangeEquations :: IsLinear a => Rule (Context (LinearSystem a))
ruleExchangeEquations = makeRule "Exchange" $ supplyLabeled2 descr args (\x y -> liftSystemTrans $ exchange x y)
 where
   descr  = ("equation 1", "equation 2")
   args c = do mv <- minvar c
               i  <- findIndex (elem mv . getVarsSystem . return) (remaining c)
               return (get covered c, get covered c + i)

ruleEliminateVar :: (Argument a, IsLinear a) => Rule (Context (LinearSystem a))
ruleEliminateVar = makeRule "Eliminate variable" $ supplyLabeled3 descr args (\x y z -> liftSystemTrans $ addEquations x y z)
 where
   descr  = ("equation 1", "equation 2", "scale factor")
   args c = do 
      mv <- minvar c
      let hd:rest = remaining c
          getCoef = coefficientOf mv . getLHS
      (i, coef) <- safeHead [ (i, c) | (i, eq) <- zip [0..] rest, let c = getCoef eq, c /= 0 ]
      guard (getCoef hd /= 0)
      let v = negate coef / getCoef hd
      return ( i + get covered c + 1, get covered c, v)

ruleDropEquation :: IsLinear a => Rule (Context (LinearSystem a))
ruleDropEquation = makeSimpleRule "Drop (0=0) equation" $ 
   \c -> do i <- findIndex (fromMaybe False . testConstants (==)) (equations c)
            return $ change covered (\n -> if i < n then n-1 else n)
                   $ fmap (deleteIndex i) c

ruleInconsistentSystem :: IsLinear a => Rule (Context (LinearSystem a))
ruleInconsistentSystem = makeSimpleRule "Inconsistent system (0=1)" $ 
   \c -> do let stop = [0 :==: 1]
            guard $ invalidSystem (equations c) && equations c /= stop
            return $ set covered 1 (fmap (const stop) c)

ruleScaleEquation :: (Argument a, IsLinear a) => Rule (Context (LinearSystem a))
ruleScaleEquation = makeRule "Scale equation to one" $ supplyLabeled2 descr args (\x y -> liftSystemTrans $ scaleEquation x y)
 where
   descr  = ("equation", "scale factor")
   args c = do eq <- safeHead $ drop (get covered c) (equations c)
               let expr = getLHS eq
               mv <- minvar c
               guard (coefficientOf mv expr /= 0)
               let coef = 1 / coefficientOf mv expr
               return (get covered c, coef)
   
ruleBackSubstitution :: (Argument a, IsLinear a) => Rule (Context (LinearSystem a))
ruleBackSubstitution = makeRule "Back substitution" $ supplyLabeled3 descr args (\x y z -> liftSystemTrans $ addEquations x y z)
 where
   descr  = ("equation 1", "equation 2", "scale factor")
   args c = do eq <- safeHead $ drop (get covered c) (equations c)
               let expr = getLHS eq
               mv <- safeHead (getVars expr)
               i  <- findIndex ((/= 0) . coefficientOf mv . getLHS) (take (get covered c) (equations c))
               let coef = negate $ coefficientOf mv (getLHS (equations c !! i))
               return (i, get covered c, coef)

ruleIdentifyFreeVariables :: IsLinear a => Rule (Context (LinearSystem a))
ruleIdentifyFreeVariables = minorRule $ makeSimpleRule "Identify free variables" $
   \c ->  let vars = [ head ys | ys <- map (getVars . getLHS) (equations c), not (null ys) ]
              change eq =
                 let (e1, e2) = splitLinearExpr (`notElem` vars) (getLHS eq) -- constant ends up in e1
                 in e2 :==: getRHS eq - e1
          in return (fmap (map change) c)

ruleCoverUpEquation :: Rule (Context (LinearSystem a))
ruleCoverUpEquation = minorRule $ makeRule "Cover up first equation" $ changeCover (+1)

ruleUncoverEquation :: Rule (Context (LinearSystem a))
ruleUncoverEquation = minorRule $ makeRule "Uncover one equation" $ changeCover (\x -> x-1)

ruleCoverAllEquations :: Rule (Context (LinearSystem a))
ruleCoverAllEquations = minorRule $ makeSimpleRule "Cover all equations" $ 
   \c -> return (set covered (length $ equations c) c)

-- local helper functions
deleteIndex :: Int -> [a] -> [a]
deleteIndex i xs = ys ++ drop 1 zs
 where (ys, zs) = splitAt i xs

testConstants :: IsLinear a => (a -> a -> Bool) -> Equation a -> Maybe Bool
testConstants f (lhs :==: rhs)
   | isConstant lhs && isConstant rhs = Just (f lhs rhs)
   | otherwise = Nothing

---------------------------------------------------------------------------------
-- Parameterized transformations

exchange :: Int -> Int -> Transformation [a]
exchange i j 
   | i >  j    = exchange j i
   | otherwise = makeTrans "exchange" $ \xs -> do
        guard (i/=j && validEquation i xs && validEquation j xs)
        let (begin, x:rest) = splitAt i xs
            (middle, y:end) = splitAt (j-i-1) rest
        return $ begin++[y]++middle++[x]++end

scaleEquation :: IsLinear a => Int -> a -> Transformation (LinearSystem a)
scaleEquation i a = makeTrans "scaleEquation" $ \xs -> do
   guard (a `notElem` [0,1] && validEquation i xs)
   let (begin, this:end) = splitAt i xs
   return (begin ++ [fmap (a*) this] ++ end)
      
addEquations :: IsLinear a => Int -> Int -> a -> Transformation (LinearSystem a)
addEquations i j a = makeTrans "addEquations" $ \xs -> do
   guard (i/=j && validEquation i xs && validEquation j xs)
   let (begin, this:end) = splitAt i xs
       exprj = xs!!j
   return $ begin++[combineWith (+) this (fmap (a*) exprj)]++end

changeCover :: (Int -> Int) -> Transformation (Context (LinearSystem a))
changeCover f = makeTrans "changeCover" $ \c -> do
   let new = f (get covered c)
   guard (new >= 0 && new <= length (equations c))
   return (set covered new c)

-- local helper function
validEquation :: Int -> [a] -> Bool
validEquation n xs = n >= 0 && n < length xs
  
--------------------
-- TEMP

equations :: Context (LinearSystem a) -> LinearSystem a
equations = fromContext

-- | The equations that remain to be solved
remaining :: Context (LinearSystem a) -> Equations a
remaining c = drop (get covered c) (equations c)

-- | The minimal variable in the remaining equations
minvar :: IsLinear a => Context (LinearSystem a) -> Maybe String
minvar c | null list = Nothing
         | otherwise = Just (minimum list)
 where
   list = getVarsSystem (remaining c) 
   
liftSystemTrans :: Transformation (LinearSystem a) -> Transformation (Context (LinearSystem a))
liftSystemTrans = lift $ makeLiftPair (return . equations) (fmap . const)

systemInNF :: (Arbitrary a, IsLinear a) => Gen (LinearSystem a)
systemInNF = do
   n <- arbitrary
   replicateM n $ liftM2 (:==:) arbitrary arbitrary

toIntegerSystem :: RealFrac a => LinearSystem a -> LinearSystem Integer
toIntegerSystem = map (fmap round)

fromIntegerSystem :: RealFrac a => LinearSystem Integer -> LinearSystem a
fromIntegerSystem = map (fmap fromInteger)