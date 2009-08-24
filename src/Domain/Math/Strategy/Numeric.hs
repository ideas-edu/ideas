module Domain.Math.Strategy.Numeric 
   ( naturalStrategy, integerStrategy
   , rationalStrategy, fractionStrategy
   , testAll
   ) where

import Prelude hiding (repeat)
import Common.Apply
import Domain.Math.Expr
import Domain.Math.Expr.Symbols
import Domain.Math.View.Numeric
import Domain.Math.View.Basic (plusView, fractionView, conView)
import Control.Monad
import Common.Strategy
import Common.Transformation
import Common.Uniplate (Uniplate, somewhereM)
import Common.View
import Test.QuickCheck

------------------------------------------------------------
-- Strategies

naturalStrategy :: Strategy Expr
naturalStrategy = repeat $ alternatives $ map swRule
   [ calcPlusWith     "nat" natView
   , calcMinusWith    "nat" natView
   , calcTimesWith    "nat" natView
   , calcDivisionWith "nat" natView
   , doubleNegate
   , minusZero
   , plusNegateLeft
   , plusNegateRight
   , minusNegateLeft
   , minusNegateRight
   , timesNegateLeft
   , timesNegateRight   
   , divisionNegateLeft
   , divisionNegateRight  
   ]
 where
   natView = makeView f fromInteger
    where
      f (Nat n) = Just n
      f _       = Nothing

integerStrategy :: Strategy Expr
integerStrategy = repeat $ alternatives $ map swRule
   [ calcPlusWith     "int" integerNormalForm
   , calcMinusWith    "int" integerNormalForm
   , calcTimesWith    "int" integerNormalForm
   , calcDivisionWith "int" integerNormalForm
   , doubleNegate
   , minusZero
   ]

rationalStrategy :: Strategy Expr
rationalStrategy = repeat $ alternatives $ map swRule
   [ calcPlusWith     "rational" rationalRelaxedForm
   , calcMinusWith    "rational" rationalRelaxedForm
   , calcTimesWith    "rational" rationalRelaxedForm
   , calcDivisionWith "int"      integerNormalForm
   , doubleNegate
   , minusZero
   , divisionNegateLeft
   , divisionNegateRight
   , divisionNumerator
   , divisionDenominator   
   , zeroNumerator
   , simplerFraction
   ]

fractionStrategy :: Strategy Expr
fractionStrategy = repeat $ alternatives $ map swRule
   [ fractionPlus, fractionPlusScale, fractionTimes
   , calcPlusWith     "int" integerNormalForm
   , calcMinusWith    "int" integerNormalForm
   , calcTimesWith    "int" integerNormalForm
   , calcDivisionWith "int" integerNormalForm
   , doubleNegate
   , minusZero
   , divisionNegateLeft
   , divisionNegateRight
   , divisionNumerator
   , divisionDenominator   
   , zeroNumerator
   , simplerFraction
   ]

swRule :: Uniplate a => Rule a -> Rule a
swRule r = makeSimpleRuleList (name r) (somewhereM (applyAll r))

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

minusZero :: Rule Expr 
minusZero = makeSimpleRule "minus zero" f
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
   f ((a :/: b) :/: c) = Just (a :/: (b :*: c))
   f _                = Nothing

divisionDenominator :: Rule Expr
divisionDenominator = makeSimpleRule "division denominator" f
 where
   f (a :/: (b :/: c)) = Just ((a :*: c) :/: b)
   f _                 = Nothing

zeroNumerator :: Rule Expr
zeroNumerator = makeSimpleRule "zero numerator" f
 where
   f (Nat 0 :/: _) = Just 0
   f _             = Nothing

simplerFraction :: Rule Expr
simplerFraction = makeSimpleRule "simpler fraction" $ \expr -> do
   new <- canonical rationalRelaxedForm expr -- also signs?????
   guard (expr /= new)
   return new

fractionPlus :: Rule Expr -- also minus
fractionPlus = makeSimpleRule "fraction plus" $ \expr -> do
   (e1, e2) <- match plusView expr
   (a, b) <- match fractionView e1
   (c, d) <- match fractionView e2
   guard (b == d)
   return (build fractionView (a+c, b))

fractionPlusScale :: Rule Expr -- also minus
fractionPlusScale = makeSimpleRuleList "fraction plus scale" $ \expr -> do
   (e1, e2) <- matchM plusView expr
   (a, b)   <- (matchM fractionView e1 `mplus` liftM (\n -> (n, 1)) (matchM conView e1)) -- conView matches -0 !!!
   (c, d)   <- (matchM fractionView e2 `mplus` liftM (\n -> (n, 1)) (matchM conView e2))
   guard (b /= 0 && d /= 0)
   let bd  = lcm b d
       e1n = build fractionView (a * (bd `div` b), bd)
       e2n = build fractionView (c * (bd `div` d), bd)
   [ build plusView (e1n, e2) | b /= bd ] ++ [
     build plusView (e1, e2n) | d /= bd ]

fractionTimes :: Rule Expr
fractionTimes = makeSimpleRule "fraction times" f 
 where
   f (e1 :*: e2) = do
      (a, b)   <- (matchM fractionView e1 `mplus` liftM (\n -> (n, 1)) (matchM conView e1)) -- conView matches -0 !!!
      (c, d)   <- (matchM fractionView e2 `mplus` liftM (\n -> (n, 1)) (matchM conView e2))
      return (build fractionView (a*c, b*d)) 
   f _ = Nothing

------------------------------------------------------------
-- Test code

testAll :: IO ()
testAll = sequence_ [test1, test2, test3, test4]

test1 = quickCheck $ forAll (sized integerGenerator) $ \e -> 
   Prelude.not (e `belongsTo` integerView) || 
   applyD naturalStrategy e `belongsTo` integerNormalForm
   
test2 = quickCheck $ forAll (sized integerGenerator) $ \e -> 
   Prelude.not (e `belongsTo` integerView) || 
   applyD integerStrategy e `belongsTo` integerNormalForm
   
test3 = quickCheck $ forAll (sized rationalGenerator) $ \e -> 
   Prelude.not (e `belongsTo` rationalView) || 
   applyD rationalStrategy e `belongsTo` rationalNormalForm
   
test4 = quickCheck $ forAll (sized rationalGenerator) $ \e -> 
   Prelude.not (e `belongsTo` rationalView) || 
   applyD fractionStrategy e `belongsTo` rationalNormalForm