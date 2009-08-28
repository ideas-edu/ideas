module Domain.Math.Numeric.Rules where

import Common.Transformation
import Control.Monad
import Domain.Math.Expr
import Domain.Math.Expr.Symbols
import Domain.Math.View.Basic

------------------------------------------------------------
-- Rules

calcRuleName :: String -> String -> String
calcRuleName opName viewName =
   "calculate " ++ opName ++ " [" ++ viewName ++ "]"
      
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
   do (e1, e2) <- isDiv e
      a <- match v e1
      b <- match v e2
      let (d, m) = divMod a b
      guard (b /= 0 && m == 0)
      return (build v d)

negateZero :: Rule Expr 
negateZero = makeSimpleRule "negate zero" f
 where
   f (Negate (Nat n)) | n == 0 = Just 0
   f _                         = Nothing

doubleNegate :: Rule Expr 
doubleNegate = makeSimpleRule "double negate" f
 where
   f (Negate (Negate a)) = Just a
   f _                   = Nothing

plusNegateLeft :: Rule Expr
plusNegateLeft = makeSimpleRule "plus negate left" f
 where
   f (Negate a :+: b) = Just (b :-: a)
   f _                = Nothing

plusNegateRight :: Rule Expr
plusNegateRight = makeSimpleRule "plus negate right" f
 where
   f (a :+: Negate b) = Just (a :-: b)
   f _                = Nothing

minusNegateLeft :: Rule Expr
minusNegateLeft = makeSimpleRule "minus negate left" f
 where
   f (Negate a :-: b) = Just (Negate (a :+: b))
   f _                = Nothing

minusNegateRight :: Rule Expr
minusNegateRight = makeSimpleRule "minus negate right" f
 where
   f (a :-: Negate b) = Just (a :+: b)
   f _                = Nothing

timesNegateLeft :: Rule Expr
timesNegateLeft = makeSimpleRule "times negate left" f
 where
   f (Negate a :*: b) = Just (Negate (a :*: b))
   f _                = Nothing

timesNegateRight :: Rule Expr
timesNegateRight = makeSimpleRule "times negate right" f
 where
   f (a :*: Negate b) = Just (Negate (a :*: b))
   f _                = Nothing

divisionNegateLeft :: Rule Expr
divisionNegateLeft = makeSimpleRule "division negate left" f
 where
   f (Negate a :/: b) = Just (Negate (a :/: b))
   f _                = Nothing

divisionNegateRight :: Rule Expr
divisionNegateRight = makeSimpleRule "division negate right" f
 where
   f (a :/: Negate b) = Just (Negate (a :/: b))
   f _                = Nothing

divisionNumerator :: Rule Expr
divisionNumerator = makeSimpleRule "division numerator" f
 where
   f ((a :/: b) :/: c)        = Just (a :/: (b :*: c))
   f (Negate (a :/: b) :/: c) = Just (Negate (a :/: (b :*: c)))
   f _                        = Nothing

divisionDenominator :: Rule Expr
divisionDenominator = makeSimpleRule "division denominator" f
 where
   f (a :/: (b :/: c))        = Just ((a :*: c) :/: b)
   f (a :/: Negate (b :/: c)) = Just (Negate ((a :*: c) :/: b))
   f _                        = Nothing

simplerFraction :: Rule Expr
simplerFraction = makeSimpleRule "simpler fraction" $ \expr -> do
   new <- canonical rationalRelaxedForm expr
   guard (expr /= new)
   return new

fractionPlus :: Rule Expr -- also minus
fractionPlus = makeSimpleRule "fraction plus" $ \expr -> do
   (e1, e2) <- match plusView expr
   (a, b)   <- match fractionForm e1
   (c, d)   <- match fractionForm e2
   guard (b == d)
   return (build fractionForm (a+c, b))

fractionPlusScale :: Rule Expr -- also minus
fractionPlusScale = makeSimpleRuleList "fraction plus scale" $ \expr -> do
   (e1, e2) <- matchM plusView expr
   (a, b)   <- (matchM fractionForm e1 `mplus` liftM (\n -> (n, 1)) (matchM integerNormalForm e1))
   (c, d)   <- (matchM fractionForm e2 `mplus` liftM (\n -> (n, 1)) (matchM integerNormalForm e2))
   guard (b /= 0 && d /= 0)
   let bd  = lcm b d
       e1n = build fractionForm (a * (bd `div` b), bd)
       e2n = build fractionForm (c * (bd `div` d), bd)
   [ build plusView (e1n, e2) | b /= bd ] ++ [
     build plusView (e1, e2n) | d /= bd ]

fractionTimes :: Rule Expr
fractionTimes = makeSimpleRule "fraction times" f 
 where
   f (e1 :*: e2) = do
      (a, b)   <- (matchM fractionForm e1 `mplus` liftM (\n -> (n, 1)) (matchM integerNormalForm e1))
      (c, d)   <- (matchM fractionForm e2 `mplus` liftM (\n -> (n, 1)) (matchM integerNormalForm e2))
      return (build fractionForm (a*c, b*d)) 
   f _ = Nothing