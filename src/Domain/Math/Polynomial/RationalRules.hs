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
module Domain.Math.Polynomial.RationalRules 
   ( divisionIsZero, divisionIsOne, sameDivisor, sameDividend
   , crossMultiply, multiplyOneDiv, fractionPlus, cancelTermsDiv
   , fractionScale, turnIntoFraction, checkSolution
   ) where

import Common.Context
import Common.Id
import Common.Transformation
import Common.View
import Control.Monad
import Data.Maybe
import Domain.Logic.Formula hiding (disjunctions, Var)
import Domain.Logic.Views
import Domain.Math.Clipboard
import Domain.Math.Data.OrList
import Domain.Math.Data.Relation
import Domain.Math.Equation.CoverUpRules
import Domain.Math.Expr
import Domain.Math.Numeric.Views
import Domain.Math.Polynomial.CleanUp
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
divisionIsZero = makeSimpleRule (ratId, "division-zero") $ withCM $ \(lhs :==: rhs) -> do
   guard (rhs == 0)
   (a, b) <- matchM divView lhs
   conditionNotZero b
   return (a :==: 0)
   
-- a/b = 1  iff  a=b (and b/=0)
divisionIsOne :: Rule (Context (Equation Expr))
divisionIsOne = makeSimpleRule (ratId, "division-one") $ withCM $ \(lhs :==: rhs) -> do
   guard (rhs == 1)
   (a, b) <- matchM divView lhs
   conditionNotZero b
   return (a :==: b)

-- a/c = b/c  iff  a=b (and c/=0)
sameDivisor :: Rule (Context (Equation Expr))
sameDivisor = makeSimpleRule (ratId, "same-divisor") $ withCM $ \(lhs :==: rhs) -> do
   (a, c1) <- matchM divView lhs
   (b, c2) <- matchM divView rhs
   guard (c1==c2)
   conditionNotZero c1
   return (a :==: b)
   
-- a/b = a/c  iff  a=0 or b=c (and b/=0 and c/=0)
sameDividend :: Rule (Context (OrList (Equation Expr)))
sameDividend = makeSimpleRule (ratId, "same-dividend") $ withCM $ oneDisjunct $ \(lhs :==: rhs) -> do
   (a1, b) <- matchM divView lhs
   (a2, c) <- matchM divView rhs
   guard (a1==a2)
   conditionNotZero b
   conditionNotZero c
   return $ orList [a1 :==: 0, b :==: c]
   
-- a/b = c/d  iff  a*d = b*c   (and b/=0 and d/=0)
crossMultiply :: Rule (Context (Equation Expr))
crossMultiply = makeSimpleRule (ratId, "cross-multiply") $ withCM $ \(lhs :==: rhs) -> do
   (a, b) <- matchM divView lhs
   (c, d) <- matchM divView rhs
   conditionNotZero b
   conditionNotZero d
   return (a*d :==: b*c)
   
-- a/b = c  iff  a = b*c  (and b/=0)
multiplyOneDiv :: Rule (Context (Equation Expr))
multiplyOneDiv = makeSimpleRule (ratId, "multiply-one-div") $ withCM $ \(lhs :==: rhs) -> 
   f (:==:) lhs rhs `mplus` f (flip (:==:)) rhs lhs
 where
   f eq ab c = do 
      guard (not (c `belongsTo` divView))
      (a, b) <- matchM divView ab
      conditionNotZero b
      return (a `eq` (b*c))
      
-- a/c + b/c = a+b/c   (also see Numeric.Rules)
fractionPlus :: Rule Expr -- also minus
fractionPlus = makeSimpleRule (ratId, "rational-plus") $ \expr -> do
   ((a, b), (c, d)) <- match myView expr
   guard (b == d)
   return (build divView (a+c, b))
 where
   myView = plusView >>> (divView *** divView)

-- ab/ac  =>  b/c  (if a/=0)
-- Note that the common term can be squared (in one of the parts)
cancelTermsDiv :: Rule (Context Expr)
cancelTermsDiv = makeSimpleRule (ratId, "cancel-div") $ withCM $ \expr -> do
   ((b, xs), (c, ys)) <- matchM myView expr
   let (ps, qs, rs) = rec (map f xs) (map f ys)
   guard (not (null rs))
   conditionNotZero (build productView (False, map g rs))
   return $ build myView ((b, map g ps), (c, map g qs))
 where
   myView = divView >>> (productView *** productView)
   powInt = simplePowerView >>> second integerView
   f a = fromMaybe (a, 1) (match powInt a)
   g a = build powInt a
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
fractionScale = liftRule myView $ 
   makeSimpleRule (ratId, "rational-scale") $ \((a, e1), (b, e2)) -> do
      guard (e1 /= e2)
      let e3 = lcmExpr e1 e2
      ma <- divisionExpr e3 e1
      mb <- divisionExpr e3 e2
      guard (ma /= 1 || mb /= 1)
      return $ ((ma*a, e3), (mb*b, e3))
 where
   myView = plusView >>> (divView *** divView)
   
turnIntoFraction :: Rule Expr
turnIntoFraction = liftRule plusView $
   makeSimpleRule (ratId, "to-rational") $ \(a, b) ->
      liftM (\c -> (c, b)) (f a b) `mplus` 
      liftM (\c -> (a, c)) (f b a)
 where
   f a b = do
      guard (not (a `belongsTo` divView))
      (_, e) <- match divView b
      return $ build divView (a*e, e)

-- A simple implementation that considers the condition stored in the context
checkSolution :: Rule (Context (OrList (Equation Expr)))
checkSolution = makeSimpleRule (ratId, "check-solution") $ 
   withCM $ oneDisjunct $ \(x :==: a) -> do
      c  <- lookupClipboardG "condition"
      xs <- matchM andView c
      guard ((x ./=. a) `elem` xs)
      return false

---------------------------------------------------------------
-- Helper-code
   
condition :: Logic (Relation Expr) -> ContextMonad ()
condition c = do
   mp <- maybeOnClipboardG "condition"
   let a = maybe id (.&&.) mp c
   unless (a==T) (addToClipboardG "condition" a)

conditionNotZero :: Expr -> ContextMonad ()
conditionNotZero expr = condition (f xs)
 where
   f  = pushNotWith (Logic.Var . notRelation) . nott
   eq = expr :==: 0
   xs = fmap (build equationView . fmap cleanUpExpr) $ 
        case match higherDegreeEquationsView (return eq) of
           Just ys -> build orListView (coverUpOrs (build higherDegreeEquationsView ys))
           Nothing -> Logic.Var (coverUp eq)