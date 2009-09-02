module Domain.Math.Equation.CoverUpRules 
   ( coverUpRules, coverUpRulesOr
   , coverUpPower, coverUpPlus, coverUpTimes, coverUpNegate
   , coverUpNumerator, coverUpDenominator, coverUpSqrt 
     -- parameterized rules
   , ConfigCoverUp(..), varConfig
   , coverUpPowerWith, coverUpPlusWith, coverUpTimesWith, coverUpNegateWith
   , coverUpNumeratorWith, coverUpDenominatorWith, coverUpSqrtWith
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

coverUpBinaryRule :: String -> (Expr -> [(Expr, Expr)]) -> (Expr -> Expr -> Expr) 
                  -> ConfigCoverUp -> Rule (Equation Expr)
coverUpBinaryRule opName fm fb cfg = 
   let name = coverUpRuleName opName (configName cfg)
   in makeSimpleRuleList name $ \(lhs :==: rhs) -> do
         (e1, e2) <- fm lhs
         guard (predicateCovered  cfg e1)
         guard (predicateCombined cfg e2)
         guard (predicateCombined cfg rhs)
         return (e1 :==: fb rhs e2)
      
coverUpUnaryRule :: String -> (Expr -> [Expr]) -> (Expr -> Expr) 
               -> ConfigCoverUp -> Rule (Equation Expr)
coverUpUnaryRule opName fm fb cfg = 
   let name = coverUpRuleName opName (configName cfg)
   in makeSimpleRuleList name $ \(lhs :==: rhs) -> do
         e1 <- fm lhs
         guard (predicateCovered  cfg e1)
         guard (predicateCombined cfg rhs)
         return (e1 :==: fb rhs)

---------------------------------------------------------------------
-- Configuration for cover-up rules

data ConfigCoverUp = Config
   { configName        :: String
   , predicateCovered  :: Expr -> Bool
   , predicateCombined :: Expr -> Bool
   }

-- default configuration
varConfig :: ConfigCoverUp 
varConfig = Config
   { configName        = "vars"
   , predicateCovered  = hasVars
   , predicateCombined = noVars
   }

---------------------------------------------------------------------
-- Parameterized cover-up rules

coverUpPowerWith :: ConfigCoverUp -> Rule (OrList (Equation Expr))
coverUpPowerWith cfg = 
   let name = coverUpRuleName "power" (configName cfg)
   in makeSimpleRule name $ onceJoinM $ \(lhs :==: rhs) -> do
         (e1, e2) <- isBinary powerSymbol lhs
         n <- isNat e2
         guard (n > 0)
         guard (predicateCovered  cfg e1)
         guard (predicateCombined cfg e2)
         guard (predicateCombined cfg rhs)
         new1 <- canonical identity (makeRoot n rhs)
         new2 <- canonical identity (negate (makeRoot n rhs))
         return $ OrList $ (e1 :==: new1) : [ e1 :==: new2 | new1 /= new2, even n ]

coverUpPlusWith :: ConfigCoverUp -> Rule (Equation Expr)
coverUpPlusWith = coverUpBinaryRule "plus" (commOp . matchM plusView) (-)

coverUpTimesWith :: ConfigCoverUp -> Rule (Equation Expr)
coverUpTimesWith = coverUpBinaryRule "times" (map signs . commOp . matchM timesView) (/)
 where
   signs (Negate x, y) = (x, neg y) -- puts negation at combined term
   signs (x, y) = (x, y)

coverUpNegateWith :: ConfigCoverUp -> Rule (Equation Expr)
coverUpNegateWith = coverUpUnaryRule "negate" isNegate negate

coverUpNumeratorWith :: ConfigCoverUp -> Rule (Equation Expr)
coverUpNumeratorWith = coverUpBinaryRule "numerator" (matchM divView) (*)

coverUpDenominatorWith :: ConfigCoverUp -> Rule (Equation Expr)
coverUpDenominatorWith = coverUpBinaryRule "denominator" (flipOp . matchM divView) (flip (/))

coverUpSqrtWith :: ConfigCoverUp -> Rule (Equation Expr)
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

coverUpPower       = coverUpPowerWith       varConfig
coverUpPlus        = coverUpPlusWith        varConfig
coverUpTimes       = coverUpTimesWith       varConfig
coverUpNegate      = coverUpNegateWith      varConfig
coverUpNumerator   = coverUpNumeratorWith   varConfig
coverUpDenominator = coverUpDenominatorWith varConfig
coverUpSqrt        = coverUpSqrtWith        varConfig

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