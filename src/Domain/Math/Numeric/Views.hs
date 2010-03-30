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
module Domain.Math.Numeric.Views
   ( integralView, realView
   , integerView, rationalView, doubleView, mixedFractionView
   , integerNormalForm, rationalNormalForm, mixedFractionNormalForm
   , rationalRelaxedForm, fractionForm
   , intDiv, fracDiv, exprToNum
   ) where

import Common.View
import Control.Monad
import Data.Ratio
import Domain.Math.Expr

-------------------------------------------------------------------
-- Numeric views

integralView :: Integral a => View Expr a
integralView = makeView (exprToNum f) fromIntegral
 where
   f s [x, y] 
      | s == divideSymbol = 
           intDiv x y
      | s == powerSymbol = do
           guard (y >= 0)
           return (x Prelude.^ y)
   f _ _ = Nothing

realView :: RealFrac a => View Expr a
realView = makeView (exprToNum f) (fromRational . toRational)
 where
   f s [x, y] 
      | s == divideSymbol = 
           fracDiv x y
      | s == powerSymbol = do
           let ry = toRational y
           guard (denominator ry == 1)
           let a = x Prelude.^ abs (numerator ry)
           return (if numerator ry < 0 then 1/a else a)
   f _ _ = Nothing
   
integerView :: View Expr Integer
integerView = integralView

rationalView :: View Expr Rational
rationalView = makeView (match realView) fromRational

mixedFractionView :: View Expr Rational
mixedFractionView = makeView (match realView) mix 
 where
   mix r = 
      let (d, m) = abs (numerator r) `divMod` denominator r
          rest   = fromInteger m ./. fromInteger (denominator r)
          sign   = if numerator r < 0 then neg else id
      in sign (fromInteger d .+. rest)

doubleView :: View Expr Double
doubleView = makeView rec Number
 where
   rec expr =
      case expr of
         Sym s xs -> mapM rec xs >>= doubleSym s
         Number d -> return d
         _        -> exprToNumStep rec expr
 
-------------------------------------------------------------------
-- Numeric views in normal form 

-- N or -N (where n is a natural number)
integerNormalForm :: View Expr Integer
integerNormalForm = makeView (optionNegate f) fromInteger
 where
   f (Nat n) = Just n
   f _       = Nothing

-- 5, -(2/5), (-2)/5, but not 2/(-5), 6/8, or -((-2)/5)
rationalNormalForm :: View Expr Rational
rationalNormalForm = makeView f fromRational
 where   
   f (Nat a :/: Nat b) = simple a b
   f (Negate (Nat a :/: Nat b)) = fmap negate (simple a b)
   f (Negate (Nat a) :/: Nat b) = fmap negate (simple a b)
   f a = fmap fromInteger (match integerNormalForm a)
   
   simple a b
      | a > 0 && b > 1 && gcd a b == 1 = 
           Just (fromInteger a / fromInteger b)
      | otherwise = Nothing

mixedFractionNormalForm :: View Expr Rational
mixedFractionNormalForm = makeView f fromRational
 where
   f (Negate (Nat a) :-: (Nat b :/: Nat c)) | a > 0 = fmap (negate . (fromInteger a+)) (simple b c)
   f (Negate (Nat a :+: (Nat b :/: Nat c))) | a > 0 = fmap (negate . (fromInteger a+)) (simple b c)
   f (Nat a :+: (Nat b :/: Nat c)) | a > 0 = fmap (fromInteger a+) (simple b c)
   f (Nat a :/: Nat b) = simple a b
   f (Negate (Nat a :/: Nat b)) = fmap negate (simple a b)
   f (Negate (Nat a) :/: Nat b) = fmap negate (simple a b)
   f a = fmap fromInteger (match integerNormalForm a)
   
   simple a b
      | a > 0 && b > 1 && gcd a b == 1 && a < b = 
           Just (fromInteger a / fromInteger b)
      | otherwise = Nothing

fractionForm :: View Expr (Integer, Integer)
fractionForm = makeView f (\(a, b) -> (fromInteger a :/: fromInteger b))
 where
   f (Negate a) = liftM (first negate) (g a)
   f a = g a
   g (e1 :/: e2) = do
      a <- match integerNormalForm e1
      b <- match integerNormalForm e2
      guard (b /= 0)
      return (a, b)
   g _       = Nothing

rationalRelaxedForm :: View Expr Rational
rationalRelaxedForm = makeView (optionNegate f) fromRational
 where
   f (e1 :/: e2) = do
      a <- match integerNormalForm e1
      b <- match integerNormalForm e2
      fracDiv (fromInteger a) (fromInteger b)
   f (Nat n) = Just (fromInteger n)
   f _       = Nothing

-- helper-function
optionNegate :: (MonadPlus m, Num a) => (Expr -> m a) -> Expr -> m a
optionNegate f (Negate a) = do b <- f a; guard (b /= 0); return (negate b)
optionNegate f a          = f a

-------------------------------------------------------------------
-- Helper functions

doubleSym :: Symbol -> [Double] -> Maybe Double
doubleSym s [x, y] 
   | s == divideSymbol = fracDiv x y
   | s == powerSymbol  = floatingPower x y   
   | s == rootSymbol && x >= 0 && y >= 1 = Just (x ** (1/y))
doubleSym _ _ = Nothing

-- General numeric interpretation function: constructors Sqrt and
-- (:/:) are interpreted with function
exprToNum :: (Monad m, Num a) => (Symbol -> [a] -> m a) -> Expr -> m a
exprToNum f = rec 
 where
   rec expr =
      case expr of
         Sym s xs -> mapM rec xs >>= f s
         _        -> exprToNumStep rec expr

exprToNumStep :: (Monad m, Num a) => (Expr -> m a) -> Expr -> m a
exprToNumStep rec expr = 
   case expr of 
      a :+: b  -> liftM2 (+)    (rec a) (rec b)
      a :*: b  -> liftM2 (*)    (rec a) (rec b)
      a :-: b  -> liftM2 (-)    (rec a) (rec b)
      Negate a -> liftM  negate (rec a)
      Nat n    -> return (fromInteger n)
      a :/: b  -> rec (Sym divideSymbol [a, b])
      Sqrt a   -> rec (Sym rootSymbol [a, 2])
      _        -> fail "exprToNumStep"

intDiv :: Integral a => a -> a -> Maybe a
intDiv x y 
   | y /= 0 && m == 0 = Just d
   | otherwise        = Nothing
 where (d, m) = x `divMod` y
 
fracDiv :: Fractional a => a -> a -> Maybe a
fracDiv x y 
   | y /= 0    = Just (x / y)
   | otherwise = Nothing
   
floatingPower :: (Ord a, Floating a) => a -> a -> Maybe a
floatingPower x y 
   | x==0 && y<0 = Nothing
   | otherwise   = Just (x**y)