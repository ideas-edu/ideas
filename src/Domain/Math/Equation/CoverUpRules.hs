-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Domain.Math.Equation.CoverUpRules 
   ( coverUpRules, coverUpRulesOr
   , coverUpPower, coverUpPlus, coverUpMinusLeft, coverUpMinusRight 
   , coverUpTimes, coverUpNegate
   , coverUpNumerator, coverUpDenominator, coverUpSqrt 
     -- parameterized rules
   , ConfigCoverUp, configName, predicateCovered, predicateCombined
   , coverLHS, coverRHS, configCoverUp, varConfig
   , coverUpPowerWith, coverUpTimesWith, coverUpNegateWith
   , coverUpPlusWith, coverUpMinusLeftWith, coverUpMinusRightWith
   , coverUpNumeratorWith, coverUpDenominatorWith, coverUpSqrtWith
   ) where

import Common.View
import Domain.Math.Expr
import Domain.Math.Data.Relation
import Control.Monad.Identity
import Common.Transformation
import Domain.Math.Expr.Symbols
import Domain.Math.Data.OrList
import Common.Traversable
import Domain.Math.Expr.Symbolic

---------------------------------------------------------------------
-- Constructors for cover-up rules

coverUpBinary2Rule :: (OnceJoin f, Switch f) => String -> (Expr -> [(Expr, Expr)]) 
                   -> (Expr -> Expr -> [f Expr])
                   -> ConfigCoverUp -> Rule (f (Equation Expr))
coverUpBinary2Rule opName fm fb cfg = 
   makeSimpleRuleList name $ onceJoinM $ \eq -> 
      (guard (coverLHS cfg) >> coverLeft eq) ++ 
      (guard (coverRHS cfg) >> coverRight eq)
 where
   name       = coverUpRuleName opName (configName cfg)
   coverRight = map (fmap flipSides) . coverLeft . flipSides
   
   coverLeft (lhs :==: rhs) = do
      (e1, e2) <- fm lhs
      guard (predicateCovered  cfg e1)
      new <- fb rhs e2
      switch $ fmap (guard . predicateCombined cfg) new
      return (fmap (e1 :==:) new)

coverUpBinaryRule :: String -> (Expr -> [(Expr, Expr)]) -> (Expr -> Expr -> Expr) 
                  -> ConfigCoverUp -> Rule (Equation Expr)
coverUpBinaryRule opName fm fb =
   let lp = makeLiftPair (return . Identity) (const . runIdentity) 
       fbi x y = [Identity (fb x y)]
   in lift lp . coverUpBinary2Rule opName fm fbi
      
coverUpUnaryRule :: String -> (Expr -> [Expr]) -> (Expr -> Expr) 
               -> ConfigCoverUp -> Rule (Equation Expr)
coverUpUnaryRule opName fm fb = 
   coverUpBinaryRule opName (map (\e -> (e, e)) . fm) (const . fb) 

coverUpRuleName :: String -> Maybe String -> String
coverUpRuleName opName viewName =
   "cover-up " ++ opName ++ maybe "" (\s -> " [" ++ s ++ "]") viewName

---------------------------------------------------------------------
-- Configuration for cover-up rules

data ConfigCoverUp = Config
   { configName        :: Maybe String
   , predicateCovered  :: Expr -> Bool
   , predicateCombined :: Expr -> Bool
   , coverLHS          :: Bool
   , coverRHS          :: Bool
   }

configCoverUp :: ConfigCoverUp
configCoverUp = Config
   { configName        = Nothing
   , predicateCovered  = const True
   , predicateCombined = const True
   , coverLHS          = True
   , coverRHS          = True
   }

-- default configuration
varConfig :: ConfigCoverUp 
varConfig = configCoverUp
   { configName        = Just "vars"
   , predicateCovered  = hasVars
   , predicateCombined = noVars
   }

---------------------------------------------------------------------
-- Parameterized cover-up rules

coverUpPowerWith :: ConfigCoverUp -> Rule (OrList (Equation Expr))
coverUpPowerWith = coverUpBinary2Rule "power" (isBinary powerSymbol) fb
 where
   fb rhs e2 = do
      n <- isNat e2
      guard (n > 0)
      new1 <- canonicalM identity (makeRoot n rhs)
      new2 <- canonicalM identity (negate (makeRoot n rhs))
      return $ orList $ new1 : [ new2 | new1 /= new2, even n ]
      
coverUpPlusWith :: ConfigCoverUp -> Rule (Equation Expr)
coverUpPlusWith = coverUpBinaryRule "plus" (commOp . isPlus) (-)

coverUpMinusLeftWith :: ConfigCoverUp -> Rule (Equation Expr)
coverUpMinusLeftWith = coverUpBinaryRule "minus left" isMinus (+)

coverUpMinusRightWith :: ConfigCoverUp -> Rule (Equation Expr)
coverUpMinusRightWith = coverUpBinaryRule "minus right" (flipOp . isMinus) (flip (-))

-- | Negations are pushed inside
coverUpTimesWith :: ConfigCoverUp -> Rule (Equation Expr)
coverUpTimesWith = coverUpBinaryRule "times" (map signs . commOp . matchM timesView) (/)
 where
   signs (Negate x, y) = (x, neg y) -- puts negation at combined term
   signs (x, y) = (x, y)

coverUpNegateWith :: ConfigCoverUp -> Rule (Equation Expr)
coverUpNegateWith = coverUpUnaryRule "negate" isNegate negate

-- | Negations are pushed inside
coverUpNumeratorWith :: ConfigCoverUp -> Rule (Equation Expr)
coverUpNumeratorWith = coverUpBinaryRule "numerator" (matchM divView) (*)

-- | Negations are pushed inside
coverUpDenominatorWith :: ConfigCoverUp -> Rule (Equation Expr)
coverUpDenominatorWith = coverUpBinaryRule "denominator" (flipOp . matchM divView) (flip (/))

coverUpSqrtWith :: ConfigCoverUp -> Rule (Equation Expr)
coverUpSqrtWith = coverUpUnaryRule "square root" isSqrt (\x -> x*x)
 where
   isSqrt (Sqrt a) = return a
   isSqrt _        = []

---------------------------------------------------------------------
-- Cover-up rules for variables

coverUpRulesOr :: [Rule (OrList (Equation Expr))]
coverUpRulesOr = coverUpPower : map ruleOnce coverUpRules

coverUpRules :: [Rule (Equation Expr)]
coverUpRules = 
   [ coverUpPlus, coverUpMinusLeft, coverUpMinusRight, coverUpNegate
   , coverUpTimes, coverUpNumerator, coverUpDenominator, coverUpSqrt
   ]

coverUpPower :: Rule (OrList (Equation Expr))
coverUpPlus, coverUpMinusLeft, coverUpMinusRight, coverUpTimes, coverUpNegate, 
   coverUpNumerator, coverUpDenominator, coverUpSqrt :: Rule (Equation Expr)

coverUpPower       = coverUpPowerWith       varConfig
coverUpPlus        = coverUpPlusWith        varConfig
coverUpMinusLeft   = coverUpMinusLeftWith   varConfig
coverUpMinusRight  = coverUpMinusRightWith  varConfig
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