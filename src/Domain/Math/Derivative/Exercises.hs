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
   , status       = Experimental
   , parser       = parseExpr
   , ready        = predicate noDiff
   , strategy     = derivativeStrategy
   , ruleOrdering = derivativeOrdering
   , navigation   = navigator
   , examples     = level Medium $ concat (diffSet3++diffSet4++
                            diffSet5++diffSet6++diffSet7++diffSet8)
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
eqPolyDiff x y =
   let f a = fromMaybe (descend f a) $ 
                apply ruleDerivPolynomial a
              `mplus`
                apply ruleDerivCon a -- rule needed because of check on var
   in viewEquivalent (polyViewWith rationalView) (f x) (f y)

eqQuotientDiff :: Expr -> Expr -> Bool
eqQuotientDiff a b = eqSimplifyRational (make a) (make b)
 where
   make = inContext derivativeQuotientExercise . f
   rs   = [ ruleDerivPolynomial, ruleDerivQuotient, ruleDerivProduct
          , ruleDerivNegate, ruleDerivPlus, ruleDerivMin
          ]
   f x  = case mapMaybe (`apply` x) rs of
             hd:_ -> f hd
             []   -> descend f x

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

{-
evalDiff :: Expr -> Expr
evalDiff expr
   | isDiff expr =
        case concatMap (`applyAll` expr) list of
           hd:_ -> evalDiff hd
           _    -> expr
   | otherwise = descend evalDiff expr
 where
   list = [ ruleDerivPolynomial, ruleDerivPowerFactor
          , ruleDerivPlus, ruleDerivMin, ruleDerivNegate
          , ruleDerivProduct, ruleDerivQuotient
          , ruleDerivPowerChain, ruleDerivSqrtChain, ruleDerivRoot
          ]

go = checkExercise derivativePowerExercise

raar i = printDerivation derivativePowerExercise expr
 where
   expr = examples derivativePowerExercise !! i

eqApprox :: Expr -> Expr -> Bool
eqApprox a b = rec 5 doubleList
 where
   vs = nub (collectVars a ++ collectVars b)

   rec 0 = const True
   rec n = rec2 n 10

   rec2 _ 0 ds = undefined -- a==b
   rec2 n m ds = case eqApproxWith f a b of
                    Just b  -> b && rec (n-1) ys
                    Nothing -> rec2 n (m-1) ys
    where
      (xs, ys) = splitAt (length vs) ds
      f = (xs !!) . fromMaybe 0 . (`elemIndex` vs)

eqApproxWith :: (String -> Double) -> Expr -> Expr -> Maybe Bool
eqApproxWith f a b = do
   d1 <- match doubleView (subst a)
   d2 <- match doubleView (subst b)
   return $ abs (d1 - d2) < 1e-9 -- 11 is still ok for example set
 where
    subst (Var s) = fromDouble (f s)
    subst expr    = descend subst expr

doubleList :: [Double] -- between -20 and 20
doubleList = iterate next (pi*exp 1)
  where
    next :: Double -> Double
    next a = if b > 20 then b-20 else b
     where
       b = a + exp 3 * log 2 -}