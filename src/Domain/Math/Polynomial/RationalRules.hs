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
module Domain.Math.Polynomial.RationalRules
   ( divisionIsZero, divisionIsOne, sameDivisor, sameDividend
   , crossMultiply, multiplyOneDiv, fractionPlus, cancelTermsDiv
   , fractionScale, turnIntoFraction, checkSolution
   ) where

import Common.Library
import Control.Monad
import Data.Maybe
import Domain.Logic.Formula hiding (Var)
import Domain.Logic.Views
import Domain.Math.CleanUp
import Domain.Math.Data.OrList
import Domain.Math.Data.Relation
import Domain.Math.Equation.CoverUpRules
import Domain.Math.Expr
import Domain.Math.Numeric.Views
import Domain.Math.Polynomial.LeastCommonMultiple
import Domain.Math.Polynomial.Views
import Domain.Math.Power.Views
import qualified Domain.Logic.Formula as Logic

ratId :: Id
ratId = newId "algebra.equations.rational"

---------------------------------------------------------------
-- Rules for rational expressions and rational equations

-- a/b = 0  iff  a=0 (and b/=0)
divisionIsZero :: Rule (Context (Equation Expr))
divisionIsZero = makeRule (ratId, "division-zero") $ \ceq -> do
   lhs :==: rhs <- current ceq
   guard (rhs == 0)
   (a, b) <- match divView lhs
   return $ conditionNotZero b
          $ replace (a :==: 0) ceq

-- a/b = 1  iff  a=b (and b/=0)
divisionIsOne :: Rule (Context (Equation Expr))
divisionIsOne = makeRule (ratId, "division-one") $ \ceq -> do
   lhs :==: rhs <- current ceq
   guard (rhs == 1)
   (a, b) <- match divView lhs
   return $ conditionNotZero b
          $ replace (a :==: b) ceq

-- a/c = b/c  iff  a=b (and c/=0)
sameDivisor :: Rule (Context (Equation Expr))
sameDivisor = makeRule (ratId, "same-divisor") $ \ceq -> do
   lhs :==: rhs <- current ceq
   (a, c1) <- match divView lhs
   (b, c2) <- match divView rhs
   guard (c1==c2)
   return $ conditionNotZero c1
          $ replace (a :==: b) ceq

-- a/b = a/c  iff  a=0 or b=c (and b/=0 and c/=0)
sameDividend :: Rule (Context (OrList (Equation Expr)))
sameDividend = makeRule (ratId, "same-dividend") $ \cor -> do
   oreq <- current cor
   lhs :==: rhs <- getSingleton oreq
   (a1, b) <- matchM divView lhs
   (a2, c) <- matchM divView rhs
   guard (a1==a2)
   let new = singleton (a1 :==: 0) <> singleton (b :==: c)
   return $ conditionNotZero c  
          $ conditionNotZero b
          $ replace new cor

-- a/b = c/d  iff  a*d = b*c   (and b/=0 and d/=0)
crossMultiply :: Rule (Context (Equation Expr))
crossMultiply = makeRule (ratId, "cross-multiply") $ \ceq -> do
   lhs :==: rhs <- current ceq
   (a, b) <- match divView lhs
   (c, d) <- match divView rhs
   return $ conditionNotZero d
          $ conditionNotZero b
          $ replace (a*d :==: b*c) ceq

-- a/b = c  iff  a = b*c  (and b/=0)
multiplyOneDiv :: Rule (Context (Equation Expr))
multiplyOneDiv = ruleList (ratId, "multiply-one-div") $ \ceq -> do
   lhs :==: rhs <- current ceq
   f (:==:) lhs rhs ceq `mplus` f (flip (:==:)) rhs lhs ceq
 where
   f eq ab c ceq = do
      guard (not (c `belongsTo` divView))
      (a, b) <- matchM divView ab
      return $ conditionNotZero b
             $ replace (a `eq` (b*c)) ceq

-- a/c + b/c = a+b/c   (also see Numeric.Rules)
fractionPlus :: Rule Expr -- also minus
fractionPlus = makeRule (ratId, "rational-plus") $ \expr -> do
   ((a, b), (c, d)) <- match myView expr
   guard (b == d)
   return (build divView (a+c, b))
 where
   myView = plusView >>> (divView *** divView)

-- ab/ac  =>  b/c  (if a/=0)
-- Note that the common term can be squared (in one of the parts)
cancelTermsDiv :: Rule (Context Expr)
cancelTermsDiv = makeRule (ratId, "cancel-div") $ \ce -> do
   expr <- current ce
   ((b, xs), (c, ys)) <- match myView expr
   let (ps, qs, rs) = rec (map f xs) (map f ys)
       new = build myView ((b, map g ps), (c, map g qs))
   guard (not (null rs))
   return $ conditionNotZero (build productView (False, map g rs))
          $ replace new ce
 where
   myView = divView >>> toView (productView *** productView)
   powInt = powerView >>> second integerView
   f a = fromMaybe (a, 1) (match powInt a)
   g   = build powInt
   rec ((_, 0):xs) ys = rec xs ys
   rec (pair@(a, n):xs) ys =
      case break ((==a) . fst) ys of
         (ys1, (b, m):ys2)
            | m == 0 ->
                 rec (pair:xs) (ys1++ys2)
            | otherwise ->
                 let i = n `min` m
                     (ps,qs,rs) = rec ((a, n-i):xs) (ys1++(b,m-i):ys2)
                 in (ps, qs, (a,i):rs)
         _ ->
            let (ps,qs,rs) = rec xs ys
            in (pair:ps, qs,rs)
   rec xs ys = (xs, ys, [])

fractionScale :: Rule Expr
fractionScale = liftView myView $
   makeRule (ratId, "rational-scale") $ \((a, e1), (b, e2)) -> do
      guard (e1 /= e2)
      let e3 = lcmExpr e1 e2
      ma <- divisionExpr e3 e1
      mb <- divisionExpr e3 e2
      guard (ma /= 1 || mb /= 1)
      return ((ma*a, e3), (mb*b, e3))
 where
   myView = plusView >>> (divView *** divView)

turnIntoFraction :: Rule Expr
turnIntoFraction = liftView plusView $
   makeRule (ratId, "to-rational") $ \(a, b) ->
      liftM (\c -> (c, b)) (f a b) `mplus`
      liftM (\c -> (a, c)) (f b a)
 where
   f a b = do
      guard (not (a `belongsTo` divView))
      (_, e) <- match divView b
      return $ build divView (a*e, e)

-- A simple implementation that considers the condition stored in the context
checkSolution :: Rule (Context (OrList (Equation Expr)))
checkSolution = makeRule (ratId, "check-solution") $ \cor -> do
   oreq <- current cor
   x :==: a <- getSingleton oreq
   c  <- lookupClipboardG "condition" cor
   xs <- match andView c
   guard ((x ./=. a) `elem` xs)
   return $ replace false cor

---------------------------------------------------------------
-- Helper-code

condition :: Logic (Relation Expr) -> Context a -> Context a
condition p c
   | new == T  = {- removeClipboardC "condition" -} c
   | otherwise = addToClipboardG "condition" new c
 where
   mp  = lookupClipboardG "condition" c
   new = maybe id (.&&.) mp p

conditionNotZero :: Expr -> Context a -> Context a
conditionNotZero expr = condition (f xs)
 where
   f  = pushNotWith (Logic.Var . notRelation) . Not
   eq = expr :==: 0
   xs = fmap (build equationView . fmap cleanUpExpr) $
        case match higherDegreeEquationsView (singleton eq) of
           Just ys -> build orListView (coverUpOrs (build higherDegreeEquationsView ys))
           Nothing -> Logic.Var (coverUp eq)