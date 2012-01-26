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
module Domain.Math.Numeric.Rules where

import Common.Library
import Control.Monad
import Domain.Math.Expr
import Domain.Math.Numeric.Views

------------------------------------------------------------
-- Rules

alg :: String
alg = "algebra.manipulation"

calcRuleName :: String -> String -> String
calcRuleName opName viewName =
   "arithmetic.operation." ++ viewName ++ "." ++ opName

calcBinRule :: String -> (a -> a -> a) -> (e -> Maybe (e, e)) -> String -> View e a -> Rule e
calcBinRule opName op m viewName v =
   makeSimpleRule (calcRuleName opName viewName) $ \e ->
   do (e1, e2) <- m e
      a <- match v e1
      b <- match v e2
      return (build v (op a b))

calcPlusWith :: Num a => String -> View Expr a -> Rule Expr
calcPlusWith = calcBinRule "plus" (+) isPlus

calcMinusWith :: Num a => String -> View Expr a -> Rule Expr
calcMinusWith = calcBinRule "minus" (-) isMinus

calcTimesWith :: Num a => String -> View Expr a -> Rule Expr
calcTimesWith = calcBinRule "times" (*) isTimes

calcDivisionWith :: Integral a => String -> View Expr a -> Rule Expr
calcDivisionWith viewName v =
   makeSimpleRule (calcRuleName "division" viewName) $ \e ->
   do (e1, e2) <- isDivide e
      a <- match v e1
      b <- match v e2
      let (d, m) = divMod a b
      guard (b /= 0 && m == 0)
      return (build v d)

negateZero :: Rule Expr
negateZero = makeSimpleRule (alg, "negate-zero") f
 where
   f (Negate (Nat n)) | n == 0 = Just 0
   f _                         = Nothing

doubleNegate :: Rule Expr
doubleNegate = makeSimpleRule (alg, "double-negate") f
 where
   f (Negate (Negate a)) = Just a
   f _                   = Nothing

plusNegateLeft :: Rule Expr
plusNegateLeft = makeSimpleRule (alg, "plus-negate-left") f
 where
   f (Negate a :+: b) = Just (b :-: a)
   f _                = Nothing

plusNegateRight :: Rule Expr
plusNegateRight = makeSimpleRule (alg, "plus-negate-right") f
 where
   f (a :+: Negate b) = Just (a :-: b)
   f _                = Nothing

minusNegateLeft :: Rule Expr
minusNegateLeft = makeSimpleRule (alg, "minus-negate-left") f
 where
   f (Negate a :-: b) = Just (Negate (a :+: b))
   f _                = Nothing

minusNegateRight :: Rule Expr
minusNegateRight = makeSimpleRule (alg, "minus-negate-right") f
 where
   f (a :-: Negate b) = Just (a :+: b)
   f _                = Nothing

timesNegateLeft :: Rule Expr
timesNegateLeft = makeSimpleRule (alg, "times-negate-left") f
 where
   f (Negate a :*: b) = Just (Negate (a :*: b))
   f _                = Nothing

timesNegateRight :: Rule Expr
timesNegateRight = makeSimpleRule (alg, "times-negate-right") f
 where
   f (a :*: Negate b) = Just (Negate (a :*: b))
   f _                = Nothing

divisionNegateLeft :: Rule Expr
divisionNegateLeft = makeSimpleRule (alg, "division-negate-left") f
 where
   f (Negate a :/: b) = Just (Negate (a :/: b))
   f _                = Nothing

divisionNegateRight :: Rule Expr
divisionNegateRight = makeSimpleRule (alg, "division-negate-right") f
 where
   f (a :/: Negate b) = Just (Negate (a :/: b))
   f _                = Nothing

divisionNumerator :: Rule Expr
divisionNumerator = makeSimpleRule (alg, "division-numerator") f
 where
   f ((a :/: b) :/: c)        = Just (a :/: (b :*: c))
   f (Negate (a :/: b) :/: c) = Just (Negate (a :/: (b :*: c)))
   f _                        = Nothing

divisionDenominator :: Rule Expr
divisionDenominator = makeSimpleRule (alg, "division-denominator") f
 where
   f (a :/: (b :/: c))        = Just ((a :*: c) :/: b)
   f (a :/: Negate (b :/: c)) = Just (Negate ((a :*: c) :/: b))
   f _                        = Nothing

simplerFraction :: Rule Expr
simplerFraction = makeSimpleRule (alg, "simpler-fraction") $ \expr -> do
   new <- canonical rationalRelaxedForm expr
   guard (expr /= new)
   return new

fractionPlus :: Rule Expr -- also minus
fractionPlus = makeSimpleRule (alg, "fraction-plus") $ \expr -> do
   (e1, e2) <- match plusView expr
   (a, b)   <- match fractionForm e1
   (c, d)   <- match fractionForm e2
   guard (b == d)
   return (build fractionForm (a+c, b))

fractionPlusScale :: Rule Expr -- also minus
fractionPlusScale = makeSimpleRuleList (alg, "fraction-plus-scale") $ \expr -> do
   (e1, e2) <- matchM plusView expr
   (a, b)   <- matchM fractionForm e1 `mplus` liftM (\n -> (n, 1)) (matchM integerNF e1)
   (c, d)   <- matchM fractionForm e2 `mplus` liftM (\n -> (n, 1)) (matchM integerNF e2)
   guard (b /= 0 && d /= 0 && b /= d)
   let bd  = lcm b d
       e1n = build fractionForm (a * (bd `div` b), bd)
       e2n = build fractionForm (c * (bd `div` d), bd)
   [ build plusView (e1n, e2) | b /= bd ] ++ [
     build plusView (e1, e2n) | d /= bd ]

fractionTimes :: Rule Expr
fractionTimes = makeSimpleRule (alg, "fraction-times") f
 where
   f (e1 :*: e2) = do
      (a, b)   <- matchM fractionForm e1 `mplus` liftM (\n -> (n, 1)) (matchM integerNF e1)
      (c, d)   <- matchM fractionForm e2 `mplus` liftM (\n -> (n, 1)) (matchM integerNF e2)
      return (build fractionForm (a*c, b*d))
   f _ = Nothing