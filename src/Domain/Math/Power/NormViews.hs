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
     normPowerView, normPowerMapView, normPowerNonNegRatio
   , normPowerNonNegDouble
   ) where

import Prelude hiding ((^), recip)
import qualified Prelude
import Control.Monad
import Common.View
import Data.List
import qualified Data.Map as M
import Domain.Math.Expr
import Domain.Math.Numeric.Views
import Domain.Math.Power.Utils

type PowerMap = (M.Map String Rational, Rational)


normPowerNonNegRatio :: View Expr (M.Map String Rational, Rational) -- (Rational, M.Map String Rational)
normPowerNonNegRatio = makeView (liftM swap . f) (g . swap)
  where
    f expr = 
        case expr of
           Sym s [a,b] 
              | isPowerSymbol s -> do
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
              | isRootSymbol s ->
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
          | isPowerSymbol s -> do
            (x, m) <- f a
            y      <- match rationalView b
            return (x ** fromRational y, M.map (*y) m)
          | isRootSymbol s -> f (Sym powerSymbol [a, 1/b])
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
              | isPowerSymbol s -> do
                   (s, r) <- f x
                   r2 <- match rationalView y
                   return (s, r*r2)
              | isRootSymbol s -> 
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

