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
   , integerView, integerNF
     -- * Decimal fractions
   , decimalFractionView
     -- * Rational numbers
   , rationalView, rationalNF
   , rationalRelaxedForm, fractionForm
     -- * Mixed fractions
   , mixedFractionView, mixedFractionNF
     -- * Double
   , doubleView, doubleNF
   ) where

import Common.Id
import Common.View
import Control.Monad
import Data.Ratio
import Domain.Math.Expr hiding ((^))
import Domain.Math.Safe
import qualified Domain.Math.Data.DecimalFraction as DF 
import qualified Domain.Math.Data.MixedFraction   as MF

-------------------------------------------------------------------
-- Natural numbers

-- |Non-negative numbers only, also for intermediate results
naturalView :: View Expr Integer
naturalView = "num.natural" @> makeView rec (fromInteger . abs)
 where
   rec :: Expr -> Maybe Integer
   rec expr = do
      x <- matchInteger rec expr
      guard (x >= 0)
      return x

naturalNF :: View Expr Integer
naturalNF = "num.natural.nf" @> makeView f (build naturalView)
 where
   f (Nat n) = Just n
   f _       = Nothing

-------------------------------------------------------------------
-- Integers

integerView :: View Expr Integer
integerView = "num.integer" @> makeView (fix matchInteger) fromIntegral

-- N or -N (where n is a natural number)
integerNF :: View Expr Integer
integerNF = "num.integer.nf" @> makeView (optionNegate f) fromInteger
 where
   f (Nat n) = Just n
   f _       = Nothing

matchInteger :: (Expr -> Maybe Integer) -> Expr -> Maybe Integer
matchInteger f expr = 
   case expr of
      a :/: b -> join (liftM2 safeDiv (f a) (f b))
      Sym s [a, b]
         | isPowerSymbol s -> join (liftM2 safePower (f a) (f b))
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
decimalFractionView = "num.decimal" @> makeView (fix matchDecimal) f
 where
   f = fromDouble . fromRational . toRational

matchDecimal :: (Expr -> Maybe DF.DecimalFraction) -> Expr -> Maybe DF.DecimalFraction
matchDecimal f expr =
   case expr of
      Number d -> Just (DF.fromDouble d)
      a :/: b  -> join (liftM2 safeDiv (f a) (f b))
      Sym s [a, b]
         | isPowerSymbol s -> join (liftM2 safePower (f a) (f b))
      Sym s [a, b, c] 
         | isMixedFractionSymbol s -> f (a+b/c)
      _ -> matchNum f expr

-------------------------------------------------------------------
-- Rational numbers

-- |like  the original defintion, except that this view
-- now also converts floating point numbers (using an exact approximation)
rationalView :: View Expr Rational
rationalView = describe "Interpret an expression as a (normalized) rational \
   \number, performing computations such as addition and multiplication if \
   \necessary." $
   "number.rational" @> makeView f fromRational
 where 
   f a = matchExact a >>= either (const Nothing) Just

matchRational :: (Expr -> Maybe Rational) -> Expr -> Maybe Rational
matchRational f expr = 
   case expr of 
      Number d -> return $ fromRational $ toRational $ DF.fromDouble d
      a :/: b  -> join (liftM2 safeDiv (f a) (f b))
      Sym s [a, b]
         | isPowerSymbol s -> join (liftM2 safePower (f a) (f b))
      Sym s [a, b, c] 
         | isMixedFractionSymbol s -> f (a+b/c)
      _ -> matchNum f expr 

matchExact :: Expr -> Maybe (Either Double Rational)
matchExact expr = 
   fmap Left (match doubleNF expr) `mplus` 
   fmap Right (fix matchRational expr)

-- 5, -(2/5), (-2)/5, but not 2/(-5), 6/8, or -((-2)/5)
rationalNF :: View Expr Rational
rationalNF = "num.rational.nf" @> makeView f fromRational
 where   
   f (Nat a :/: Nat b) = simpleRational a b
   f (Negate (Nat a :/: Nat b)) = fmap negate (simpleRational a b)
   f (Negate (Nat a) :/: Nat b) = fmap negate (simpleRational a b)
   f a = fmap fromInteger (match integerNF a)

simpleRational :: Integer -> Integer -> Maybe Rational
simpleRational a b = do
   guard (a > 0 && b > 1 && gcd a b == 1)
   return (fromInteger a / fromInteger b)

fractionForm :: View Expr (Integer, Integer)
fractionForm = "num.fraction-form" @> makeView f g
 where
   f = match (divView >>> integerNF *** integerNF)
   g (a, b) = fromInteger a :/: fromInteger b

rationalRelaxedForm :: View Expr Rational
rationalRelaxedForm = "num.rational-relaxed" @> makeView (optionNegate f) fromRational
 where
   f (e1 :/: e2) = do
      a <- match integerNF e1
      b <- match integerNF e2
      safeDiv (fromInteger a) (fromInteger b)
   f (Nat n) = Just (fromInteger n)
   f _       = Nothing

-------------------------------------------------------------------
-- Mixed fractions

mixedFractionView :: View Expr MF.MixedFraction
mixedFractionView = "num.mixed-fraction" @> makeView f (sign g)
 where
   f = fmap fromRational . fix matchRational
   
   sign k a | a < 0     = negate (k (abs a))
            | otherwise = k a
   
   g mixed 
      | frac  == 0 = fromInteger  whole
      | whole == 0 = fromRational frac
      | otherwise  = function mixedFractionSymbol $ map fromInteger parts
    where
      whole = MF.wholeNumber mixed
      frac  = MF.fractionPart mixed
      parts = [whole, numerator frac, denominator frac]

mixedFractionNF :: View Expr MF.MixedFraction
mixedFractionNF = describe "A normal form for mixed fractions. \
   \Improper fractions (numerator greater or equal to denominator) are not \
   \allowed." $
   "number.mixed-fraction.nf" @> makeView f (build mixedFractionView)
 where
   f (Sym s [Nat a, Nat b, Nat c]) 
      | isMixedFractionSymbol s = simple a b c   
   f (Negate (Sym s [Nat a, Nat b, Nat c]))
      | isMixedFractionSymbol s = liftM negate (simple a b c)
   f expr = do r <- match rationalNF expr
               guard ((-1 < r && r < 1) || denominator r == 1)
               return (fromRational r)
   
   simple a b c = do
      guard (a > 0 && b > 0 && b < c)
      r <- simpleRational b c
      return (fromInteger a + fromRational r)

-------------------------------------------------------------------
-- Double (imprecise floating-points)

doubleView :: View Expr Double
doubleView = "num.double" @> makeView (fix matchDouble) fromDouble
 
doubleNF :: View Expr Double
doubleNF = "num.double.nf" @> makeView (optionNegate f) fromDouble
 where
   f (Number d) = Just d
   f _          = Nothing
 
matchDouble :: (Expr -> Maybe Double) -> Expr -> Maybe Double
matchDouble f expr =
   case expr of
      Number d -> Just d
      a :/: b  -> join (liftM2 safeDiv (f a) (f b))
      Sqrt a   -> f a >>= safeSqrt
      Sym s [a, b]
         | isPowerSymbol s -> join (liftM2 safePower (f a) (f b))
         | isRootSymbol s  -> join (liftM2 safeRoot (f a) (f b))
      Sym s [a, b, c] 
         | isMixedFractionSymbol s -> f (a+b/c)
      _ -> matchNum f expr

-------------------------------------------------------------------
-- Helper functions

optionNegate :: Num a => (Expr -> Maybe a) -> Expr -> Maybe a
optionNegate f (Negate a) = do b <- f a; guard (b /= 0); return (negate b)
optionNegate f a          = f a

fix :: (a -> a) -> a
fix f = f (fix f)