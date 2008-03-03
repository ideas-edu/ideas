module Domain.LinearAlgebra.EquationsRules where

import Prelude hiding (repeat)
import Common.Apply
import Common.Context
import Common.Transformation
import Common.Utils
import Common.Unification
import Control.Monad
import Data.List hiding (repeat)
import Data.Maybe
import qualified Data.Set as S
import Domain.LinearAlgebra.Equation
import Domain.LinearAlgebra.LinearExpr
import Domain.LinearAlgebra.LinearSystem
import Domain.LinearAlgebra.MatrixRules (covered) -- for context
import Test.QuickCheck -- hopefully, temporarily

equationsRules :: (Read a, Fractional a) => [Rule (EqsInContext a)]
equationsRules = [ ruleExchangeEquations, ruleEliminateVar, ruleDropEquation, ruleInconsistentSystem
                 , ruleScaleEquation, ruleBackSubstitution, ruleIdentifyFreeVariables
                 , ruleCoverUpEquation, ruleUncoverEquation, ruleCoverAllEquations ]

ruleExchangeEquations :: Rule (EqsInContext a)
ruleExchangeEquations = makeRule "Exchange" $ app2 (\x y -> liftSystemTrans $ exchange x y) descr args 
 where
   descr  = (makeArgument "equation 1", makeArgument "equation 2")
   args c = do mv <- minvar c
               i  <- findIndex (S.member mv . getVars) (remaining c)
               return (get covered c, get covered c + i)

ruleEliminateVar :: (Read a, Fractional a) => Rule (EqsInContext a)
ruleEliminateVar = makeRule "Eliminate variable" $ app3 (\x y z -> liftSystemTrans $ addEquations x y z) descr args
 where
   descr  = (makeArgument "equation 1", makeArgument "equation 2", makeArgument "scale factor")
   args c = do 
      mv <- minvar c
      let hd:rest = remaining c
          getCoef = coefficientOf mv . getLHS
      (i, coef) <- safeHead [ (i, c) | (i, eq) <- zip [0..] rest, let c = getCoef eq, c /= 0 ]
      guard (getCoef hd /= 0)
      let v = negate coef / getCoef hd
      return ( i + get covered c + 1, get covered c, v)

ruleDropEquation :: Eq a => Rule (EqsInContext a)
ruleDropEquation = makeSimpleRule "Drop (0=0) equation" $ 
   \c -> do i <- findIndex (fromMaybe False . testConstants (==)) (equations c)
            return $ change covered (\n -> if i < n then n-1 else n)
                   $ fmap (deleteIndex i) c

ruleInconsistentSystem :: Num a => Rule (EqsInContext a)
ruleInconsistentSystem = makeSimpleRule "Inconsistent system (0=1)" $ 
   \c -> do let stop = [0 :==: 1]
            guard $ invalidSystem (equations c) && equations c /= stop
            return $ set covered 1 (fmap (const stop) c)

ruleScaleEquation :: (Read a, Fractional a) => Rule (EqsInContext a)
ruleScaleEquation = makeRule "Scale equation to one" $ app2 (\x y -> liftSystemTrans $ scaleEquation x y) descr args
 where
   descr  = (makeArgument "equation", makeArgument "scale factor")
   args c = do eq <- safeHead $ drop (get covered c) (equations c)
               let expr = getLHS eq
               mv <- safeHead (getVarsList expr)
               guard (coefficientOf mv expr /= 0)
               let coef = 1 / coefficientOf mv expr
               return (get covered c, coef)
   
ruleBackSubstitution :: (Read a, Num a) => Rule (EqsInContext a)
ruleBackSubstitution = makeRule "Back substitution" $ app3 (\x y z -> liftSystemTrans $ addEquations x y z) descr args
 where
   descr  = (makeArgument "equation 1", makeArgument "equation 2", makeArgument "scale factor")
   args c = do eq <- safeHead $ drop (get covered c) (equations c)
               let expr = getLHS eq
               mv <- safeHead (getVarsList expr)
               i  <- findIndex ((/= 0) . coefficientOf mv . getLHS) (take (get covered c) (equations c))
               let coef = negate $ coefficientOf mv (getLHS (equations c !! i))
               return (i, get covered c, coef)

ruleIdentifyFreeVariables :: Num a => Rule (EqsInContext a)
ruleIdentifyFreeVariables = minorRule $ makeSimpleRule "Identify free variables" $
   \c ->  let vars = [ head ys | ys <- map (S.toList . getVars . getLHS) (equations c), not (null ys) ]
              change eq =
                 let (e1, e2) = splitLinearExpr (`notElem` vars) (getLHS eq) -- constant ends up in e1
                 in e2 :==: getRHS eq - e1
          in return (fmap (map change) c)

ruleCoverUpEquation :: Rule (EqsInContext a)
ruleCoverUpEquation = minorRule $ makeRule "Cover up first equation" $ changeCover (+1)

ruleUncoverEquation :: Rule (EqsInContext a)
ruleUncoverEquation = minorRule $ makeRule "Uncover one equation" $ changeCover (\x -> x-1)

ruleCoverAllEquations :: Rule (EqsInContext a)
ruleCoverAllEquations = minorRule $ makeSimpleRule "Cover all equations" $ 
   \c -> return (set covered (length $ equations c) c)

-- local helper functions
deleteIndex :: Int -> [a] -> [a]
deleteIndex i xs = ys ++ drop 1 zs
 where (ys, zs) = splitAt i xs

testConstants :: (a -> a -> Bool) -> Equation (LinearExpr a) -> Maybe Bool
testConstants f eq = do
   x <- isConstant (getLHS eq)
   y <- isConstant (getRHS eq)
   return (f x y)

---------------------------------------------------------------------------------
-- Parameterized transformations

exchange :: Int -> Int -> Transformation [a]
exchange i j 
   | i >  j    = exchange j i
   | otherwise = makeTrans $ \xs -> do
        guard (i/=j)
        let (begin, x:rest) = splitAt i xs
            (middle, y:end) = splitAt (j-i-1) rest
        return $ begin++[y]++middle++[x]++end

scaleEquation :: Num a => Int -> a -> Transformation (LinearSystem a)
scaleEquation i a = makeTrans $ \xs -> do
   guard (a `notElem` [0,1])
   let (begin, this:end) = splitAt i xs
   return (begin ++ [fmap (toLinearExpr a*) this] ++ end)
      
addEquations :: Num a => Int -> Int -> a -> Transformation (LinearSystem a)
addEquations i j a = makeTrans $ \xs -> do
   guard (i/=j)
   let (begin, this:end) = splitAt i xs
       exprj = xs!!j
   return $ begin++[combineWith (+) this (fmap (toLinearExpr a*) exprj)]++end

changeCover :: (Int -> Int) -> Transformation (EqsInContext a)
changeCover f = makeTrans $ \c -> do
   let new = f (get covered c)
   guard (new >= 0 && new <= length (equations c))
   return (set covered new c)
   
--------------------
-- TEMP

type EqsInContext a = Context (LinearSystem a)

equations :: EqsInContext a -> LinearSystem a
equations = fromContext

-- | The equations that remain to be solved
remaining :: EqsInContext a -> Equations (LinearExpr a)
remaining c = drop (get covered c) (equations c)

-- | The minimal variable in the remaining equations
minvar :: EqsInContext a -> Maybe String
minvar c | S.null set = Nothing
         | otherwise  = Just (S.findMin set)
 where
   set = getVars (remaining c) 
   
liftSystemTrans :: Transformation (LinearSystem a) -> Transformation (EqsInContext a)
liftSystemTrans f = makeTrans $ \c -> do
   new <- apply f (equations c) 
   return (fmap (const new) c)

systemInNF :: (Arbitrary a, Num a) => Gen (LinearSystem a)
systemInNF = do
   n <- arbitrary
   replicateM n $ liftM2 (:==:) arbitrary (liftM toLinearExpr arbitrary)

toIntegerSystem :: RealFrac a => LinearSystem a -> LinearSystem Integer
toIntegerSystem = map (fmap (fmap round))

fromIntegerSystem :: RealFrac a => LinearSystem Integer -> LinearSystem a
fromIntegerSystem = map (fmap (fmap fromInteger))
