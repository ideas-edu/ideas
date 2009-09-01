module Domain.Math.Equation.CoverUpRules 
   ( coverUpRules, coverUpRulesOr
   , coverUpPowerWith, coverUpPlusWith, coverUpTimesWith, coverUpNegateWith
   , coverUpNumeratorWith, coverUpDenominatorWith, coverUpSqrtWith
   , coverUpPower, coverUpPlus, coverUpTimes, coverUpNegate
   , coverUpNumerator, coverUpDenominator, coverUpSqrt 
   ) where

import Domain.Math.Expr
import Domain.Math.Data.Equation
import Control.Monad
import Common.Transformation
import Domain.Math.Expr.Symbols
import Domain.Math.Data.OrList
import Domain.Math.View.Basic
import Common.Traversable
import Domain.Math.Expr.Symbolic

---------------------------------------------------------------------
-- Constructors for cover-up rules

coverUpRuleName :: String -> String -> String
coverUpRuleName opName viewName =
   "cover-up " ++ opName ++ " [" ++ viewName ++ "]"

coverUpBinaryRule :: String -> (a -> [(a, a)]) -> (a -> a -> a) 
               -> String -> (a -> Bool) -> Rule (Equation a)
coverUpBinaryRule opName fm fb s p = 
   makeSimpleRuleList (coverUpRuleName opName s) $ \(lhs :==: rhs) -> do
      (e1, e2) <- fm lhs
      guard (p e1 && not (p e2) && not (p rhs))
      return (e1 :==: fb rhs e2)
      
coverUpUnaryRule :: String -> (a -> [a]) -> (a -> a) 
               -> String -> (a -> Bool) -> Rule (Equation a)
coverUpUnaryRule opName fm fb s p = 
   makeSimpleRuleList (coverUpRuleName opName s) $ \(lhs :==: rhs) -> do
      e1 <- fm lhs
      guard (p e1 && not (p rhs))
      return (e1 :==: fb rhs)

---------------------------------------------------------------------
-- Parameterized cover-up rules
 
coverUpPowerWith :: String -> (Expr -> Bool) -> Rule (OrList (Equation Expr))
coverUpPowerWith s p = makeSimpleRule (coverUpRuleName "power" s) $ 
   onceJoinM $ \(lhs :==: rhs) -> do
      (e1, e2) <- isBinary powerSymbol lhs
      n <- isNat e2
      guard (p e1 && n > 0 && not (p e2) && not (p rhs))
      new1 <- canonical identity (makeRoot n rhs)
      new2 <- canonical identity (negate (makeRoot n rhs))
      return $ OrList $ (e1 :==: new1) : [ e1 :==: new2 | new1 /= new2, even n ]

coverUpPlusWith :: String -> (Expr -> Bool) -> Rule (Equation Expr)
coverUpPlusWith = coverUpBinaryRule "plus" (commOp . matchM plusView) (-)

coverUpTimesWith :: String -> (Expr -> Bool) -> Rule (Equation Expr)
coverUpTimesWith = coverUpBinaryRule "times" (commOp . isTimes) (/)

coverUpNegateWith :: String -> (Expr -> Bool) -> Rule (Equation Expr)
coverUpNegateWith = coverUpUnaryRule "negate" isNegate negate

coverUpNumeratorWith :: String -> (Expr -> Bool) -> Rule (Equation Expr)
coverUpNumeratorWith = coverUpBinaryRule "numerator" (matchM divView) (*)

coverUpDenominatorWith :: String -> (Expr -> Bool) -> Rule (Equation Expr)
coverUpDenominatorWith = coverUpBinaryRule "denominator" (flipOp . matchM divView) (flip (/))

coverUpSqrtWith :: String -> (Expr -> Bool) -> Rule (Equation Expr)
coverUpSqrtWith = coverUpUnaryRule "square root" isSqrt (\x -> x*x)

---------------------------------------------------------------------
-- Cover-up rules for variables

coverUpRulesOr :: [Rule (OrList (Equation Expr))]
coverUpRulesOr = coverUpPower : map ruleOnce coverUpRules

coverUpRules :: [Rule (Equation Expr)]
coverUpRules = 
   [ coverUpPlus, coverUpTimes, coverUpNegate
   , coverUpNumerator, coverUpDenominator, coverUpSqrt
   ]

coverUpPower :: Rule (OrList (Equation Expr))
coverUpPlus, coverUpTimes, coverUpNegate, 
   coverUpNumerator, coverUpDenominator, coverUpSqrt :: Rule (Equation Expr)

coverUpPower       = coverUpPowerWith       "var" hasVars
coverUpPlus        = coverUpPlusWith        "var" hasVars
coverUpTimes       = coverUpTimesWith       "var" hasVars
coverUpNegate      = coverUpNegateWith      "var" hasVars
coverUpNumerator   = coverUpNumeratorWith   "var" hasVars
coverUpDenominator = coverUpDenominatorWith "var" hasVars
coverUpSqrt        = coverUpSqrtWith        "var" hasVars

---------------------------------------------------------------------
-- Some helper-functions

commOp :: MonadPlus m => m (a, a) -> m (a, a)
commOp m = do 
   (a, b) <- m 
   return (a, b) `mplus` return (b, a)

flipOp :: Monad m => m (a, a) -> m (a, a)
flipOp = liftM (\(x, y) -> (y, x))

isNat :: MonadPlus m => Expr -> m Integer
isNat (Nat n) = return n
isNat _       = mzero

makeRoot :: Integer -> Expr -> Expr
makeRoot n a 
   | n == 1    = a
   | n == 2    = sqrt a
   | otherwise = root (fromInteger n) a