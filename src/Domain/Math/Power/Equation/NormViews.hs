-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  alex.gerdes@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------

module Domain.Math.Power.Equation.NormViews
   ( normPowerEqApproxView
   , normPowerEqView
   , normExpEqView
   , normLogEqView
--   , normLogView
   ) where

import Common.Classes
import Common.Id
import Common.View
import Common.Rewriting
import Control.Arrow ( (>>^) )
import Control.Monad
import Data.List
import Data.Maybe
import Data.Ratio
import Domain.Math.Approximation
import Domain.Math.Data.OrList
import Domain.Math.Data.PrimeFactors
import Domain.Math.Data.Relation
import Domain.Math.Expr
import Domain.Math.Numeric.Views
import Domain.Math.Polynomial.CleanUp
import Domain.Math.Polynomial.Views
import Domain.Math.Power.NormViews
import Domain.Math.Power.Utils
import Domain.Math.Power.Views


-- Change to configurable strategy!
normPowerEqApproxView :: Int -> View (Relation Expr) (Expr, Expr)
normPowerEqApproxView d = makeView f (uncurry (.~=.))
   where
     f rel = case relationType rel of 
      EqualTo       ->  match (equationView >>> normPowerEqView) rel 
                    >>= return . \(l, r) -> (l, simplifyWith (precision d) doubleView r)
      Approximately ->  return (leftHandSide rel, rightHandSide rel)
      _             ->  Nothing

normPowerEqView :: View (Equation Expr) (Expr, Expr) -- with x>0!
normPowerEqView = makeView f (uncurry (:==:))
  where
    f expr = do
      -- selected var to the left, the rest to the right
      (lhs :==: rhs) <- varLeft expr >>= constRight
      -- match power
      (c, ax)        <- match (timesView <&> (identity >>^ (,) 1)) $
                          simplify normPowerView lhs
      (a, x)         <- match myPowerView ax
      -- simplify, scale and take root
      return (a, simplify rationalView ((rhs ./. c) .^. (1 ./. x)))

    myPowerView =  powerView 
               <&> (rootView >>> second (makeView (\a->Just (1 ./. a)) (\b->1 ./. b)))
               <&> (identity >>^ \a->(a,1))

constRight (lhs :==: rhs) = do
  (vs, cs) <- match sumView lhs >>= return . partition hasSomeVar
  let rhs' = rhs .+. build sumView (map neg cs)
  return $ negateEq $ build sumView vs :==: simplifyWith normalizeSum sumView rhs'

negateEq (lhs :==: rhs) = 
  case lhs of
    Negate lhs' -> lhs' :==: neg rhs
    _           -> lhs  :==: rhs

varLeft (lhs :==: rhs) = do
  (vs, cs) <- match sumView rhs >>= return . partition hasSomeVar
  return $ lhs .+. build sumView (map neg vs) :==: build sumView cs

scaleLeft (lhs :==: rhs) = 
  match timesView lhs >>= \(c, x) -> return $ 
    x :==: simplifyWith (second normalizeProduct) productView (rhs ./. c)

normExpEqView :: View (Equation Expr) (String, Rational)
normExpEqView = makeView f id >>> linearEquationView
  where
    try f a = fromMaybe a $ f a
    f e = do
      let (l :==: r) = try scaleLeft $ try constRight e
      return $ case match powerView l of
        Just (b, x) -> x :==: simplify normLogView (logBase b r)
        Nothing     -> l :==: r

normLogEqView :: View (OrList (Equation Expr)) (OrList (Equation Expr))
normLogEqView = makeView (liftM g . switch . fmap f) id  -- AG: needs to be replaced by higherOrderEqView
  where
    f expr@(lhs :==: rhs) = return $
      case match logView lhs of
        Just (b, x) -> x :==: simplify myRationalView (b .^. rhs)
        Nothing     -> expr
    g = fmap (fmap (simplify myRationalView)) 
      . fmap (simplify normPowerEqView) 
      . simplify quadraticEquationsView 

-- liftToOrListView :: View a b -> View (OrList a) (OrList b)
-- liftToOrListView v = makeView (switch . fmap (match v)) ()

normLogView :: View Expr Expr
normLogView = makeView g id
  where
    g expr = 
      case expr of 
        Sym s [x, y] 
          | isLogSymbol s -> do
              b <- match integerView x
              let divExp (b, exp) = return $ f b y ./. Nat exp
              maybe (Just $ f b y) divExp $ greatestPower b
          | otherwise -> Nothing
        _ -> Nothing
    f b e = 
      case e of
        Nat 1     -> Nat 0
        Nat n     
          | n == b    -> Nat 1
          | otherwise -> maybe (logBase (fromInteger b) (fromInteger n)) Nat 
                       $ lookup b (allPowers n)
        e1 :*: e2 -> f b e1 .+. f b e2
        e1 :/: e2 -> f b e1 .-. f b e2
        Sqrt e    -> f b (e .^. (1 ./. 2))
        Negate e  -> Negate $ f b e
        Sym s [x,y]
          | isPowerSymbol s -> y .*. f b x
          | isRootSymbol  s -> f b (x .^. (1 ./. y))
        e         -> e

myRationalView :: View Expr Rational
myRationalView = makeView (exprToNum f) id >>> rationalView
  where
    f s [x, y] 
      | isDivideSymbol s = 
          fracDiv x y
      | isPowerSymbol s = do
          ry <- match rationalView y
          if denominator ry == 1 
            then do 
              let a = x Prelude.^ abs (numerator ry)
              return (if numerator ry < 0 then 1/a else a)
            else
              f (newId rootSymbol) [ fromInteger (denominator ry)
                           , x Prelude.^ (numerator ry) ]
      | isRootSymbol s = do
          n <- match integerView y
          b <- match integerView x
          liftM fromInteger $ lookup b $ map (\(a,b)->(b,a)) (allPowers n)
    f _ _ = Nothing

