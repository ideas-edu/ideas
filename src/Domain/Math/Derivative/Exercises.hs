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
module Domain.Math.Derivative.Exercises
   ( derivativeExercise, derivativePolyExercise
   , derivativeProductExercise, derivativeQuotientExercise
   , derivativePowerExercise
   ) where

import Common.Library
import Common.Utils.Uniplate
import Control.Monad
import Data.Function
import Data.List
import Data.Maybe
import Data.Ord
import Domain.Math.CleanUp
import Domain.Math.Derivative.Examples
import Domain.Math.Derivative.Rules
import Domain.Math.Derivative.Strategies
import Domain.Math.Expr
import Domain.Math.Numeric.Views
import Domain.Math.Polynomial.Generators
import Domain.Math.Polynomial.RationalExercises
import Domain.Math.Polynomial.Views
import Prelude hiding (repeat, (^))
import Test.QuickCheck

derivativePolyExercise :: Exercise Expr
derivativePolyExercise = describe
   "Find the derivative of a polynomial. First normalize the polynomial \
   \(e.g., with distribution). Don't make use of the product-rule, or \
   \other chain rules." $ makeExercise
   { exerciseId    = diffId # "polynomial"
   , status        = Provisional
   , parser        = parseExpr
   , ready         = predicateView (polyNormalForm rationalView)
   , suitable      = predicate isPolyDiff
   , equivalence   = withoutContext eqPolyDiff
   , similarity    = withoutContext (viewEquivalent cleanUpACView)
   , strategy      = derivativePolyStrategy
   , navigation    = navigator
   , examples      = level Medium $ concat (diffSet1 ++ diffSet2 ++ diffSet3)
   , testGenerator = Just $ liftM (diff . lambda "x") $
                        sized quadraticGen
   }
 
derivativeProductExercise :: Exercise Expr
derivativeProductExercise = describe
   "Use the product-rule to find the derivative of a polynomial. Keep \
   \the parentheses in your answer." $
   derivativePolyExercise
   { exerciseId    = diffId # "product"
   , ready         = predicate noDiff
   , strategy      = derivativeProductStrategy
   , examples      = level Medium $ concat diffSet3
   }

derivativeQuotientExercise :: Exercise Expr
derivativeQuotientExercise = describe
   "Use the quotient-rule to find the derivative of a polynomial. Only \
   \remove parentheses in the numerator." $
   derivativePolyExercise
   { exerciseId    = diffId # "quotient"
   , ready         = predicate readyQuotientDiff
   , suitable      = predicate isQuotientDiff
   , equivalence   = withoutContext eqQuotientDiff
   , strategy      = derivativeQuotientStrategy
   , ruleOrdering  = ruleOrderingWithId [ruleDerivQuotient]
   , examples      = level Medium $ concat diffSet4
   , testGenerator = Nothing
   }

derivativePowerExercise :: Exercise Expr
derivativePowerExercise = describe
   "First write as a power, then find the derivative. Rewrite negative or \
   \rational exponents." $
   derivativePolyExercise
   { exerciseId    = diffId # "power"
   , status        = Experimental
   , ready         = predicate noDiff <&&> predicate onlyNatPower
   -- , isSuitable    = const True
   , equivalence   = \_ _ -> True -- \x y -> eqApprox (evalDiff x) (evalDiff y)
   , strategy      = derivativePowerStrategy
   , examples      = level Medium $ concat (diffSet5 ++ diffSet6)
   , testGenerator = Nothing
   }

derivativeExercise :: Exercise Expr
derivativeExercise = makeExercise
   { exerciseId   = describe "Derivative" diffId
   , status       = Provisional
   , parser       = parseExpr
   , ready        = predicate noDiff
   , strategy     = derivativeStrategy
   , ruleOrdering = derivativeOrdering
   , equivalence   = withoutContext eqQuotientDiff
   , navigation   = navigator
   , examples     = level Medium $ concat $ diffSet3++diffSet4++diffSet5
                            {- diffSet6 -- ++diffSet7++diffSet8 -}
   }

derivativeOrdering :: Rule a -> Rule a -> Ordering
derivativeOrdering = comparing f
 where
   f a = (getId a /= j, getId a == i, showId a)
   i = getId ruleDefRoot
   j = getId ruleDerivPolynomial

isPolyDiff :: Expr -> Bool
isPolyDiff = maybe False (`belongsTo` polyViewWith rationalView) . getDiffExpr

isQuotientDiff :: Expr -> Bool
isQuotientDiff de = fromMaybe False $ do
   expr <- getDiffExpr de
   xs   <- match sumView expr
   let f a = maybe [a] (\(x, y) -> [x, y]) (match divView a)
       ys  = concatMap f xs
       isp = (`belongsTo` polyViewWith rationalView)
   return (all isp ys)

eqPolyDiff :: Expr -> Expr -> Bool
eqPolyDiff = viewEquivalent (polyViewWith rationalView) `on` evalDiff

eqQuotientDiff :: Expr -> Expr -> Bool
eqQuotientDiff = eqSimplifyRational `on` (cleanUpExpr . evalDiff)

readyQuotientDiff :: Expr -> Bool
readyQuotientDiff expr = fromMaybe False $ do
   xs <- match sumView expr
   let f a      = fromMaybe (a, 1) (match divView a)
       (ys, zs) = unzip (map f xs)
       isp = (`belongsTo` polyViewWith rationalView)
       nfp = (`belongsTo` polyNormalForm rationalView)
   return (all nfp ys && all isp zs)

noDiff :: Expr -> Bool
noDiff e = all (not . isDiffSymbol) [ s | Sym s _ <- universe e]

onlyNatPower :: Expr -> Bool
onlyNatPower e = all isNat [ a | Sym s [_, a] <- universe e, isPowerSymbol s ]
 where
   isNat (Nat _) = True
   isNat _       = False

evalDiff :: Expr -> Expr
evalDiff da = 
   case da of 
      Sym d [Sym l [Var x, expr]] | isDiffSymbol d && isLambdaSymbol l ->
         cleanUpExpr (rec x expr)
      _ -> descend evalDiff da
 where
   rec x expr =
      case expr of
         _ | withoutVar x expr -> 0 
         Var y | x==y -> 1
         a :+: b  -> rec x a + rec x b
         a :-: b  -> rec x a - rec x b
         Negate a -> -rec x a
         a :*: b  -> rec x a*b + a*rec x b
         a :/: b  -> (b*rec x a - a*rec x b) / b^2
         Sqrt a   -> rec x (a^(1/2))
         Sym s [a, b] 
            | isPowerSymbol s -> 
                 case match rationalView b of
                    Just n  -> fromRational n * a^fromRational (n-1) * rec x a
                    Nothing -> diffExpr
            | isRootSymbol s -> 
                 rec x (a^(1/b))
         _ -> diffExpr
    where
      diffExpr = diff (lambda x expr)