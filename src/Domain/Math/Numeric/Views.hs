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
module Domain.Math.Numeric.Views
   ( -- * Natural numbers
     naturalView, naturalNF
     -- * Integers
   , integerView, integerNF, integralView 
     -- * Decimal fractions
     -- * Rest
   , rationalView, doubleView, mixedFractionView
   , rationalNormalForm, mixedFractionNormalForm
   , doubleNormalForm, rationalRelaxedForm, fractionForm, exactView
   , intDiv, fracDiv
   ) where

import Common.Id
import Common.Uniplate
import Common.View
import Control.Monad
import Data.Ratio
import Domain.Math.Expr hiding ((^))
import qualified Domain.Math.Data.DecimalFraction as DF 
import qualified Domain.Math.Data.MixedFraction   as MF

-------------------------------------------------------------------
-- Natural numbers

-- |Non-negative numbers only, also for intermediate results
naturalView :: View Expr Integer
naturalView = "num.natural" @> makeView rec fromInteger
 where
   rec :: Expr -> Maybe Integer
   rec expr = do
      x <- matchInteger rec expr
      guard (x >= 0)
      return x

naturalNF :: View Expr Integer
naturalNF = "num.natural.nf" @> makeView f fromInteger
 where
   f (Nat n) = Just n
   f _       = Nothing

-------------------------------------------------------------------
-- Integers

integerView :: View Expr Integer
integerView = integralView

-- N or -N (where n is a natural number)
integerNF :: View Expr Integer
integerNF = "num.integer.nf" @> makeView (optionNegate f) fromInteger
 where
   f (Nat n) = Just n
   f _       = Nothing

integralView :: Integral a => View Expr a
integralView = "num.integer" @> makeView (close matchInteger) fromIntegral

matchInteger :: Integral a => (Expr -> Maybe a) -> Expr -> Maybe a
matchInteger f expr = 
   case expr of
      a :/: b -> join (liftM2 intDiv (f a) (f b))
      Sym s [a, b]
         | isPowerSymbol s -> join (liftM2 intPower (f a) (f b))
      _ -> matchNum f expr

matchNum :: Num a => (Expr -> Maybe a) -> Expr -> Maybe a
matchNum f expr =
   case expr of
      Nat n    -> return (fromInteger n)
      a :+: b  -> liftM2 (+) (f a) (f b)
      a :-: b  -> liftM2 (-) (f a) (f b)
      Negate a -> liftM negate (f a)
      a :*: b  -> liftM2 (*) (f a) (f b)
      _        -> Nothing

-------------------------------------------------------------------
-- Decimal fractions

decimalFractionView :: View Expr DF.DecimalFraction
decimalFractionView = "num.decimal" @> makeView (close matchDecimal) f
 where
   f = fromDouble . fromRational . toRational

matchDecimal :: (Expr -> Maybe DF.DecimalFraction) -> Expr -> Maybe DF.DecimalFraction
matchDecimal f expr =
   case expr of
      Number d -> Just (DF.fromDouble d)
      a :/: b  -> join (liftM2 DF.divide (f a) (f b))
      Sym s [a, b]
         | isPowerSymbol s -> join (liftM2 DF.power (f a) (f b))
      _ -> matchNum f expr

-------------------------------------------------------------------
-- Numeric views

-- |like oldRationalView (the original defintion), except that this view
-- now also converts floating point numbers (using an exact approximation)
rationalView :: View Expr Rational
rationalView = describe "Interpret an expression as a (normalized) rational \
   \number, performing computations such as addition and multiplication if \
   \necessary." $
   "number.rational" @> makeView f fromRational
 where 
   f a = match exactView a >>= either (const Nothing) Just

oldRationalView :: View Expr Rational
oldRationalView = makeView (close matchRational) fromRational

matchRational :: (Expr -> Maybe Rational) -> Expr -> Maybe Rational
matchRational f expr = 
   case expr of 
      a :/: b  -> join (liftM2 fracDiv (f a) (f b))
      Sym s [a, b]
         | isPowerSymbol s -> join (liftM2 fracPower (f a) (f b))
      _ -> matchNum f expr 

exactView :: View Expr (Either Double Rational)
exactView = "number.exact" @> makeView f (either fromDouble fromRational)
 where
   f (Number d) = Just (Left d)
   f (Negate a) = fmap (negate +++ negate) (f a)
   f expr       = fmap Right (match oldRationalView (g expr))
   
   g (Number d) = fromRational (toRational (DF.fromDouble d))
   g expr       = descend g expr

mixedFractionView :: View Expr MF.MixedFraction
mixedFractionView = "num.mixed-fraction" @> makeView f g
 where
   f   = fmap fromRational . match oldRationalView
   g a = let sign = if a < 0 then negate else id
             rest = fromRational (MF.fractionPart a)
         in sign (fromInteger (MF.wholeNumber a) .+. rest)

doubleView :: View Expr Double
doubleView = "num.double" @> makeView (close matchDouble) fromDouble
 
matchDouble :: (Expr -> Maybe Double) -> Expr -> Maybe Double
matchDouble f expr =
   case expr of
      Number d -> Just d
      a :/: b  -> join (liftM2 fracDiv (f a) (f b))
      Sqrt a   -> f a >>= (`floatingRoot` 2)
      Sym s [a, b]
         | isPowerSymbol s -> join (liftM2 floatingPower (f a) (f b))
         | isRootSymbol s  -> join (liftM2 floatingRoot (f a) (f b))
      _ -> matchNum f expr
  
-------------------------------------------------------------------
-- Numeric views in normal form 



-- 5, -(2/5), (-2)/5, but not 2/(-5), 6/8, or -((-2)/5)
rationalNormalForm :: View Expr Rational
rationalNormalForm = "num.rational-nf" @> makeView f fromRational
 where   
   f (Nat a :/: Nat b) = simpleRational a b
   f (Negate (Nat a :/: Nat b)) = fmap negate (simpleRational a b)
   f (Negate (Nat a) :/: Nat b) = fmap negate (simpleRational a b)
   f a = fmap fromInteger (match integerNF a)
   
mixedFractionNormalForm :: View Expr MF.MixedFraction
mixedFractionNormalForm = describe "A normal form for mixed fractions. \
   \Improper fractions (numerator greater or equal to denominator) are not \
   \allowed." $
   "number.mixed-fraction-nf" @> makeView f (build mixedFractionView)
 where
   f (Negate (Nat a) :-: (Nat b :/: Nat c)) = fmap negate (simple a b c)
   f (Negate (Nat a :+: (Nat b :/: Nat c))) = fmap negate (simple a b c)
   f (Nat a :+: (Nat b :/: Nat c)) = simple a b c
   f expr = do r <- match rationalNormalForm expr
               guard ((-1 < r && r < 1) || denominator r == 1)
               return (fromRational r)
   
   simple a b c = do
      guard (a > 0 && b < c)
      r <- simpleRational b c
      return (fromInteger a + fromRational r)

doubleNormalForm :: View Expr Double
doubleNormalForm = "num.double-nf" @> makeView f fromDouble
 where
   f (Number d) = Just d
   f _          = Nothing

simpleRational :: Integer -> Integer -> Maybe Rational
simpleRational a b = do
   guard (a > 0 && b > 1 && gcd a b == 1)
   return (fromInteger a / fromInteger b)

fractionForm :: View Expr (Integer, Integer)
fractionForm = "num.fraction-form" @> makeView f (\(a, b) -> (fromInteger a :/: fromInteger b))
 where
   f (Negate a) = liftM (first negate) (g a)
   f a = g a
   g (e1 :/: e2) = do
      a <- match integerNF e1
      b <- match integerNF e2
      guard (b /= 0)
      return (a, b)
   g _       = Nothing

rationalRelaxedForm :: View Expr Rational
rationalRelaxedForm = "num.rational-relaxed" @> makeView (optionNegate f) fromRational
 where
   f (e1 :/: e2) = do
      a <- match integerNF e1
      b <- match integerNF e2
      fracDiv (fromInteger a) (fromInteger b)
   f (Nat n) = Just (fromInteger n)
   f _       = Nothing

-- helper-function
optionNegate :: (MonadPlus m, Num a) => (Expr -> m a) -> Expr -> m a
optionNegate f (Negate a) = do b <- f a; guard (b /= 0); return (negate b)
optionNegate f a          = f a

-------------------------------------------------------------------
-- Helper functions

close :: (a -> a) -> a
close f = f (close f)

intDiv :: Integral a => a -> a -> Maybe a
intDiv x y 
   | y /= 0 && m == 0 = Just d
   | otherwise        = Nothing
 where (d, m) = x `divMod` y
 
intPower :: (Num a, Integral b) => a -> b -> Maybe a
intPower x y 
   | y >= 0    = Just (x Prelude.^ y) 
   | otherwise = Nothing 
 
fracDiv :: Fractional a => a -> a -> Maybe a
fracDiv x y 
   | y /= 0    = Just (x / y)
   | otherwise = Nothing

fracPower :: Fractional a => a -> Rational -> Maybe a
fracPower x r
   | denominator r /= 1 = Nothing
   | y >= 0             = Just a
   | otherwise          = Just (1/a)
 where
   y = numerator r
   a = x Prelude.^ abs y

floatingPower :: (Ord a, Floating a) => a -> a -> Maybe a
floatingPower x y 
   | x==0 && y<0 = Nothing
   | otherwise   = Just (x**y)

floatingRoot :: (Ord a, Floating a) => a -> a -> Maybe a
floatingRoot x y
   | x >= 0 && y >= 1 = Just (x ** (1/y))
   | otherwise        = Nothing