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

module Domain.Math.Power.NormViews 
   ( -- * Normalising views
     normPowerView, normPowerMapView, normPowerNonNegRatio, normExpEqView
   , normPowerNonNegDouble, normPowerEqApproxView, normPowerEqView
   , normLogEqView
   ) where

import Prelude hiding ((^), recip)
import qualified Prelude
import Control.Arrow ( (>>^) )
import Control.Monad
import Common.Classes
import Common.View
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Ratio
import Domain.Math.Approximation (precision)
import Domain.Math.Data.PrimeFactors (allPowers, greatestPower)
import Domain.Math.Data.Relation
import Domain.Math.Data.OrList
import Domain.Math.Expr
import Domain.Math.Numeric.Views
import Domain.Math.Polynomial.Views (linearEquationView, quadraticEquationsView)
import Domain.Math.Polynomial.CleanUp (normalizeProduct, normalizeSum)
import Domain.Math.Power.Views
import qualified Domain.Math.Data.SquareRoot as SQ

normPowerNonNegRatio :: View Expr (M.Map String Rational, Rational) -- (Rational, M.Map String Rational)
normPowerNonNegRatio = makeView (liftM swap . f) (g . swap)
 where
     swap (x,y) = (y,x)
     f expr = 
        case expr of
           Sym s [a,b] 
              | s==powerSymbol -> do
                   (r, m) <- f a
                   if r==1 
                     then do
                       r2 <- match rationalView b
                       return (1, M.map (*r2) m)
                     else do
                       n <- match integerView b
                       if n >=0 
                         then return (r Prelude.^ n, M.map (*fromIntegral n) m)
                         else return (1/(r Prelude.^ abs n), M.map (*fromIntegral n) m)
              | s==rootSymbol ->
                  f (Sym powerSymbol [a, 1/b])
           Sqrt a -> 
              f (Sym rootSymbol [a,2])
           a :*: b -> do
             (r1, m1) <- f a
             (r2, m2) <- f b
             return (r1*r2, M.unionWith (+) m1 m2)
           a :/: b -> do
             (r1, m1) <- f a
             (r2, m2) <- f b
             guard (r2 /= 0)
             return (r1/r2, M.unionWith (+) m1 (M.map negate m2))
           Var s -> return (1, M.singleton s 1)
           Nat n -> return (toRational n, M.empty)
           Negate x -> do 
             (r, m) <- f x
             return (negate r, m)
           _ -> do
             r <- match rationalView expr
             return (fromRational r, M.empty)
     g (r, m) = 
       let xs = map f (M.toList m)
           f (s, r) = Var s .^. fromRational r
       in build productView (False, fromRational r : xs)

-- | AG: todo: change double to norm view for rationals
normPowerNonNegDouble :: View Expr (Double, M.Map String Rational)
normPowerNonNegDouble = makeView (liftM (roundof 6) . f) g
  where
    roundof n (x, m) = (fromIntegral (round (x * 10.0 ** n)) / 10.0 ** n, m)
    f expr = 
      case expr of
        Sym s [a,b] 
          | s==powerSymbol -> do
            (x, m) <- f a
            y      <- match rationalView b
            return (x ** fromRational y, M.map (*y) m)
          | s==rootSymbol -> f (Sym powerSymbol [a, 1/b])
        Sqrt a -> f (Sym rootSymbol [a,2])
        a :*: b -> do
          (r1, m1) <- f a
          (r2, m2) <- f b
          return (r1*r2, M.unionWith (+) m1 m2)
        a :/: b -> do
          (r1, m1) <- f a
          (r2, m2) <- f b
          guard (r2 /= 0)
          return (r1/r2, M.unionWith (+) m1 (M.map negate m2))
        Var s -> return (1, M.singleton s 1)
        Negate x -> do 
          (r, m) <- f x
          return (negate r, m)
        _ -> do
          d <- match doubleView expr
          return (d, M.empty)
    g (r, m) = 
      let xs = map f (M.toList m)
          f (s, r) = Var s .^. fromRational r
      in build productView (False, fromDouble r : xs)


type PowerMap = (M.Map String Rational, Rational)

normPowerMapView :: View Expr [PowerMap]
normPowerMapView = makeView (liftM h . f) g
  where
    f = (mapM (match normPowerNonNegRatio) =<<) . match sumView
    g = build sumView . map (build normPowerNonNegRatio)
    h :: [PowerMap] -> [PowerMap]
    h = map (foldr1 (\(x,y) (_,q) -> (x,y+q))) . groupBy (\x y -> fst x == fst y) . sort

normPowerView :: View Expr (String, Rational)
normPowerView = makeView f g
 where
   f expr = 
        case expr of
           Sym s [x,y] 
              | s==powerSymbol -> do
                   (s, r) <- f x
                   r2 <- match rationalView y
                   return (s, r*r2)
              | s==rootSymbol -> 
                   f (x^(1/y))
           Sqrt x ->
              f (Sym rootSymbol [x, 2])
           Var s -> return (s, 1) 
           x :*: y -> do
             (s1, r1) <- f x
             (s2, r2) <- f y
             guard (s1==s2)
             return (s1, r1+r2)
           Nat 1 :/: y -> do
             (s, r) <- f y
             return (s, -r)
           x :/: y -> do
             (s1, r1) <- f x
             (s2, r2) <- f y
             guard (s1==s2)
             return (s1, r1-r2) 
           _ -> Nothing
             
   g (s, r) = Var s .^. fromRational r

normPowerEqApproxView :: Int -> View (Relation Expr) (Expr, Expr)
normPowerEqApproxView d = makeView f (uncurry (.~=.))
   where
     f rel = case relationType rel of 
      EqualTo       ->  match (equationView >>> normPowerEqView) rel 
                    >>= return . \(l, r) -> (l, simplifyWith (precision d) doubleView r)
      Approximately ->  return (leftHandSide rel, rightHandSide rel)
      _             ->  Nothing

normPowerEqView :: View (Equation Expr) (Expr, Expr)
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

    myPowerView =  simplePowerView 
               <&> (simpleRootView >>> second (makeView (\a->Just (1 ./. a)) (\b->1 ./. b)))
               <&> (identity >>^ \a->(a,1))

constRight (lhs :==: rhs) = do
  (vs, cs) <- match sumView lhs >>= return . partition (not . null . collectVars)
  let rhs' = rhs .+. build sumView (map neg cs)
  return $ build sumView vs :==: simplifyWith normalizeSum sumView rhs'

varLeft (lhs :==: rhs) = do
  (vs, cs) <- match sumView rhs >>= return . partition (not . null . collectVars)
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
      return $ case match simplePowerView l of
        Just (b, x) -> x :==: simplify normLogView (logBase b r)
        Nothing     -> l :==: r

normLogEqView :: View (OrList (Equation Expr)) (OrList (String, SQ.SquareRoot Rational))
normLogEqView = makeView (switch . fmap f) id >>> quadraticEquationsView
  where
    f expr@(lhs :==: rhs) = do
      return $ case match logView lhs of
        Just (b, x) -> x :==: b .^. rhs
        Nothing     -> expr

normLogView :: View Expr Expr
normLogView = makeView g id
  where
    g expr = 
      case expr of 
        Sym s [x, y] 
          | s == logSymbol -> do
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
          | s == powerSymbol -> y .*. f b x
          | s == rootSymbol  -> f b (x .^. (1 ./. y))
        e         -> e

-- misschien deze naar rationalView verplaatsen?
normConstPowerView :: View Expr Integer
normConstPowerView = makeView f fromRational
  where 
    f expr = 
      case expr of
        Sym s [x, y] 
          | s == powerSymbol -> do
              base <- f
              (n, d)  <- match (divView >>> rationalView *** rationalView) y
              if denominator exp == 1 
                then return $ base Prelude.^ exp
                else f (fromRational base)
--                else f ((fromRational base) .^. (fromRational (numerator exp))) 
--                >>=           f . root (numerator exp)
          | s == rootSymbol -> do
              n <- f x
              e <- f y
              if denominator e == 1
                then liftM fromInteger $ lookup (fromRational n) $ allPowers (numerator e)
                else Nothing
        e -> match rationalView e
