-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
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
   , coverUp, coverUpOrs
   , coverUpPower, coverUpPlus, coverUpMinusLeft, coverUpMinusRight 
   , coverUpTimes, coverUpNegate
   , coverUpNumerator, coverUpDenominator, coverUpSqrt 
     -- parameterized rules
   , ConfigCoverUp, configName, predicateCovered, predicateCombined
   , coverLHS, coverRHS, configCoverUp
   , coverUpPowerWith, coverUpTimesWith, coverUpNegateWith
   , coverUpPlusWith, coverUpMinusLeftWith, coverUpMinusRightWith
   , coverUpNumeratorWith, coverUpDenominatorWith, coverUpSqrtWith
     -- temporarily exported
   , coverUpBinaryRule, commOp, flipOp
   ) where

import Common.Classes
import Common.Context
import Common.Id
import Common.Rewriting
import Common.Transformation
import Common.View
import Control.Monad
import Data.Maybe
import Data.Foldable
import Data.Traversable (Traversable, mapM)
import Domain.Math.Data.OrList
import Domain.Math.Data.Relation
import Domain.Math.Expr

---------------------------------------------------------------------
-- Constructors for cover-up rules

coverUpFunction :: (Traversable f, Relational r) 
                   => (Expr -> [(Expr, Expr)]) 
                   -> (Expr -> Expr -> [f Expr])
                   -> ConfigCoverUp -> r Expr -> [f (r Expr)]
coverUpFunction fm fb cfg eq0 = 
   (guard (coverLHS cfg) >> coverLeft eq0) ++ 
   (guard (coverRHS cfg) >> coverRight eq0)
 where
   coverRight   = map (fmap flipSides) . coverLeft . flipSides
   coverLeft eq = do
      (e1, e2) <- fm (leftHandSide eq)
      guard (predicateCovered  cfg e1)
      new <- fb (rightHandSide eq) e2
      _   <- Data.Traversable.mapM (guard . predicateCombined cfg) new
      return (fmap (constructor eq e1) new)

coverUpBinaryOrRule :: Relational r
                   => String -> (Expr -> [(Expr, Expr)]) 
                   -> (Expr -> Expr -> [OrList Expr])
                   -> ConfigCoverUp -> Rule (OrList (r Expr))
coverUpBinaryOrRule opName fm fb cfg =
   let name = coverUpRuleName opName (configName cfg)
   in makeSimpleRuleList name $ oneDisjunct $ coverUpFunction fm fb cfg
   
coverUpBinaryRule :: Relational r => String 
                  -> (Expr -> [(Expr, Expr)]) -> (Expr -> Expr -> Expr) 
                  -> ConfigCoverUp -> Rule (r Expr)
coverUpBinaryRule opName fm fb cfg = 
   let name = coverUpRuleName opName (configName cfg)
       fb2 a b = [[fb a b]]
   in makeSimpleRuleList name $ map head . coverUpFunction fm fb2 cfg 
      
coverUpUnaryRule :: Relational r => String -> (Expr -> [Expr]) -> (Expr -> Expr) 
               -> ConfigCoverUp -> Rule (r Expr)
coverUpUnaryRule opName fm fb = 
   coverUpBinaryRule opName (map (\e -> (e, e)) . fm) (const . fb) 

coverUpRuleName :: String -> String -> Id
coverUpRuleName opName cfg =
   let f = if null cfg then newId else ( cfg # )
   in "algebra.equations.coverup" # f opName

---------------------------------------------------------------------
-- Configuration for cover-up rules

data ConfigCoverUp = Config
   { configName        :: String
   , predicateCovered  :: Expr -> Bool
   , predicateCombined :: Expr -> Bool
   , coverLHS          :: Bool
   , coverRHS          :: Bool
   }

-- Default configuration: cover-up part with variables
configCoverUp :: ConfigCoverUp
configCoverUp = Config
   { configName        = ""
   , predicateCovered  = hasSomeVar
   , predicateCombined = hasNoVar
   , coverLHS          = True
   , coverRHS          = True
   }

---------------------------------------------------------------------
-- Parameterized cover-up rules

coverUpPowerWith :: ConfigCoverUp -> Rule (OrList (Equation Expr))
coverUpPowerWith = coverUpBinaryOrRule "power" (isBinary powerSymbol) fb
 where
   fb rhs e2 = do
      n <- isNat e2
      guard (n > 0)
      let new1 = root rhs (fromIntegral n)
          new2 = (neg new1)
      return $ to new1 <> if even n && new1 /= new2 then to new2 else false
      
coverUpPlusWith :: ConfigCoverUp -> Rule (Equation Expr)
coverUpPlusWith = coverUpBinaryRule "plus" (commOp . isPlus) (-)

coverUpMinusLeftWith :: ConfigCoverUp -> Rule (Equation Expr)
coverUpMinusLeftWith = coverUpBinaryRule "minus-left" isMinus (+)

coverUpMinusRightWith :: ConfigCoverUp -> Rule (Equation Expr)
coverUpMinusRightWith = coverUpBinaryRule "minus-right" (flipOp . isMinus) (flip (-))

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
coverUpSqrtWith = coverUpUnaryRule "sqrt" isSqrt (\x -> x*x)
 where
   isSqrt (Sqrt a) = return a
   isSqrt _        = []

---------------------------------------------------------------------
-- Cover-up rules for variables

coverUpOrs :: OrList (Equation Expr) -> OrList (Equation Expr)
coverUpOrs = foldMap  (f . coverUp)
 where
   f :: Equation Expr -> OrList (Equation Expr)
   f eq = case apply coverUpPower (to eq) of
             Just xs -> coverUpOrs xs
             Nothing -> to eq
                 
coverUp :: Equation Expr -> Equation Expr
coverUp eq = 
   case mapMaybe (`apply` eq) coverUpRules of
      hd:_ -> coverUp hd
      _    -> eq

coverUpRulesOr :: IsTerm a => [Rule (Context a)]
coverUpRulesOr = use coverUpPower : map use coverUpRules

coverUpRules :: [Rule (Equation Expr)]
coverUpRules = 
   [ coverUpPlus, coverUpMinusLeft, coverUpMinusRight, coverUpNegate
   , coverUpTimes, coverUpNumerator, coverUpDenominator, coverUpSqrt
   ]

coverUpPower :: Rule (OrList (Equation Expr))
coverUpPlus, coverUpMinusLeft, coverUpMinusRight, coverUpTimes, coverUpNegate, 
   coverUpNumerator, coverUpDenominator, coverUpSqrt :: Rule (Equation Expr)

coverUpPower       = coverUpPowerWith       configCoverUp
coverUpPlus        = coverUpPlusWith        configCoverUp
coverUpMinusLeft   = coverUpMinusLeftWith   configCoverUp
coverUpMinusRight  = coverUpMinusRightWith  configCoverUp
coverUpTimes       = coverUpTimesWith       configCoverUp
coverUpNegate      = coverUpNegateWith      configCoverUp
coverUpNumerator   = coverUpNumeratorWith   configCoverUp
coverUpDenominator = coverUpDenominatorWith configCoverUp
coverUpSqrt        = coverUpSqrtWith        configCoverUp

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