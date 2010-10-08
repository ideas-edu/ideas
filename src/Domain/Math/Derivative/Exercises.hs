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
module Domain.Math.Derivative.Exercises 
   ( derivativeExercise, derivativePolyExercise
   , derivativeProductExercise, derivativeQuotientExercise
   , derivativePowerExercise
   ) where

import Common.Uniplate (universe)
import Control.Monad
import Data.Maybe
import Prelude hiding (repeat, (^))
import Domain.Math.Derivative.Rules 
import Domain.Math.Derivative.Strategies
import Common.Library
import Common.Uniplate (descend)
import Domain.Math.Polynomial.Generators
import Domain.Math.Polynomial.Views
import Domain.Math.Polynomial.CleanUp
import Domain.Math.Polynomial.RationalExercises
import Domain.Math.Numeric.Views
import Domain.Math.Examples.DWO5
import Domain.Math.Expr
import Test.QuickCheck

derivativePolyExercise :: Exercise Expr
derivativePolyExercise = describe
   "Find the derivative of a polynomial. First normalize the polynomial \
   \(e.g., with distribution). Don't make use of the product-rule, or \
   \other chain rules." $ makeExercise
   { exerciseId    = diffId # "polynomial"
   , status        = Provisional
   , parser        = parseExpr
   , isReady       = (`belongsTo` polyNormalForm rationalView)
   , isSuitable    = isPolyDiff
   , equivalence   = eqPolyDiff
   , similarity    = simPolyDiff
   , strategy      = derivativePolyStrategy
   , navigation    = navigator
   , examples      = concat (diffSet1 ++ diffSet2 ++ diffSet3)
   , testGenerator = Just $ liftM (diff . lambda (Var "x")) $ 
                        sized quadraticGen
   }

derivativeProductExercise :: Exercise Expr
derivativeProductExercise = describe
   "Use the product-rule to find the derivative of a polynomial. Keep \
   \the parentheses in your answer." $ 
   derivativePolyExercise
   { exerciseId    = diffId # "product"
   , isReady       = noDiff
   , strategy      = derivativeProductStrategy
   , examples      = concat diffSet3
   }

derivativeQuotientExercise :: Exercise Expr
derivativeQuotientExercise = describe
   "Use the quotient-rule to find the derivative of a polynomial. Only \
   \remove parentheses in the numerator." $ 
   derivativePolyExercise
   { exerciseId    = diffId # "quotient"
   , isReady       = readyQuotientDiff
   , isSuitable    = isQuotientDiff
   , equivalence   = eqQuotientDiff
   , strategy      = derivativeQuotientStrategy
   , ruleOrdering  = ruleOrderingWithId [ruleDerivQuotient]
   , examples      = concat diffSet4
   , testGenerator = Nothing
   }

derivativePowerExercise :: Exercise Expr
derivativePowerExercise = describe
   "First write as a power, then find the derivative. Rewrite negative or \
   \rational exponents." $ 
   derivativePolyExercise
   { exerciseId    = diffId # "power"
   , status        = Experimental
   , isReady       = noDiff
   -- , isSuitable    = isQuotientDiff
   -- , equivalence   = eqQuotientDiff
   , strategy      = derivativePowerStrategy
   , examples      = concat (diffSet5 ++ diffSet6)
   , testGenerator = Nothing
   }

derivativeExercise :: Exercise Expr
derivativeExercise = makeExercise
   { exerciseId   = describe "Derivative" diffId
   , status       = Experimental
   , parser       = parseExpr
   , isReady      = noDiff
   , strategy     = derivativeStrategy
   , ruleOrdering = derivativeOrdering
   , navigation   = navigator
   , examples     = concat (diffSet3++diffSet4++
                            diffSet5++diffSet6++diffSet7++diffSet8)
   }

derivativeOrdering :: Rule a -> Rule b -> Ordering
derivativeOrdering x y = f x `compare` f y
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
   let f a = fromMaybe (descend f a) (apply ruleDerivPolynomial a)
   in viewEquivalent (polyViewWith rationalView) (f x) (f y)

eqQuotientDiff :: Expr -> Expr -> Bool
eqQuotientDiff a b = eqSimplifyRational (make a) (make b)
 where
   make = inContext derivativeQuotientExercise . f
   rs   = [ ruleDerivPolynomial, ruleDerivQuotient, ruleDerivProduct
          , ruleDerivNegate, ruleDerivPlus, ruleDerivMin
          ]
   f a  = case mapMaybe (`apply` a) rs of
             x:_ -> f x
             []  -> descend f a

readyQuotientDiff :: Expr -> Bool
readyQuotientDiff expr = fromMaybe False $ do
   xs <- match sumView expr
   let f a      = fromMaybe (a, 1) (match divView a) -- ANDERSOM
       (ys, zs) = unzip (map f xs)
       isp = (`belongsTo` polyViewWith rationalView)
       nfp = (`belongsTo` polyNormalForm rationalView)
   return (all nfp ys && all isp zs)

simPolyDiff :: Expr -> Expr -> Bool
simPolyDiff x y =
   let f = acExpr . cleanUpExpr2
   in f x == f y

noDiff :: Expr -> Bool
noDiff e = null [ () | Sym s _ <- universe e, s == diffSymbol ]   

go = checkExercise derivativeQuotientExercise

raar = printDerivation derivativeProductExercise expr
 where 
   x = Var "x"
   expr = diff $ lambda (Var "x")  $ (-27/2*((-13/2-x)*(85/6-x)+54/7*(x^2/(-58/7))))