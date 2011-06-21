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
   ( integralView, integerView
   , rationalView, doubleView, mixedFractionView
   , integerNormalForm, rationalNormalForm, mixedFractionNormalForm
   , doubleNormalForm, rationalRelaxedForm, fractionForm, exactView
   , intDiv, fracDiv
   ) where

import Common.Id
import Common.Rewriting
import Common.Uniplate
import Common.View
import Control.Monad
import Data.Ratio
import Domain.Math.Expr hiding ((^))
import qualified Domain.Math.Data.MixedFraction as MF

-------------------------------------------------------------------
-- Numeric views

integralView :: Integral a => View Expr a
integralView = "num.integer" @> makeView (exprToNum f (const Nothing)) fromIntegral
 where
   f s [x, y] 
      | isDivideSymbol s = 
           intDiv x y
      | isPowerSymbol s = do
           guard (y >= 0)
           return (x Prelude.^ y)
   f _ _ = Nothing
   
integerView :: View Expr Integer
integerView = integralView

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
oldRationalView = makeView (exprToNum f (const Nothing)) fromRational
 where
   f s [x, y] 
      | isDivideSymbol s = 
           fracDiv x y
      | isPowerSymbol s = do
           let ry = toRational y
           guard (denominator ry == 1)
           let a = x Prelude.^ abs (numerator ry)
           return (if numerator ry < 0 then 1/a else a)
   f _ _ = Nothing

exactView :: View Expr (Either Double Rational)
exactView = "number.exact" @> makeView f (either fromDouble fromRational)
 where
   f (Number d) = Just (Left d)
   f (Negate a) = fmap (negate +++ negate) (f a)
   f expr       = fmap Right (match oldRationalView (g expr))
   
   g (Number d) = fromRational (doubleToRational d)
   g expr       = descend g expr

doubleToRational :: Double -> Rational
doubleToRational d = fromInteger base / 10^digs
 where
   digs = 8 :: Int -- maximum number of digits
   base = round (d * 10^digs) :: Integer

mixedFractionView :: View Expr MF.MixedFraction
mixedFractionView = "num.mixed-fraction" @> makeView f g
 where
   f   = fmap fromRational . match oldRationalView
   g a = let sign = if a < 0 then negate else id
             rest = fromRational (MF.fractionPart a)
         in sign (fromInteger (MF.wholeNumber a) .+. rest)

doubleView :: View Expr Double
doubleView = "num.double" @> makeView (exprToNum doubleSym return) fromDouble
 
-------------------------------------------------------------------
-- Numeric views in normal form 

-- N or -N (where n is a natural number)
integerNormalForm :: View Expr Integer
integerNormalForm = "num.integer-nf" @> makeView (optionNegate f) fromInteger
 where
   f (Nat n) = Just n
   f _       = Nothing

-- 5, -(2/5), (-2)/5, but not 2/(-5), 6/8, or -((-2)/5)
rationalNormalForm :: View Expr Rational
rationalNormalForm = "num.rational-nf" @> makeView f fromRational
 where   
   f (Nat a :/: Nat b) = simpleRational a b
   f (Negate (Nat a :/: Nat b)) = fmap negate (simpleRational a b)
   f (Negate (Nat a) :/: Nat b) = fmap negate (simpleRational a b)
   f a = fmap fromInteger (match integerNormalForm a)
   
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
      a <- match integerNormalForm e1
      b <- match integerNormalForm e2
      guard (b /= 0)
      return (a, b)
   g _       = Nothing

rationalRelaxedForm :: View Expr Rational
rationalRelaxedForm = "num.rational-relaxed" @> makeView (optionNegate f) fromRational
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
   | isDivideSymbol s = fracDiv x y
   | isPowerSymbol  s = floatingPower x y   
   | isRootSymbol   s && x >= 0 && y >= 1 = Just (x ** (1/y))
doubleSym _ _ = Nothing

-- General numeric interpretation function: constructors Sqrt and
-- (:/:) are interpreted with function
exprToNum :: (Monad m, Num a) => (Symbol -> [a] -> m a) -> (Double -> m a) -> Expr -> m a
exprToNum f g = rec 
 where
   rec expr =
      case expr of
         Sym s xs -> mapM rec xs >>= f s
         Number d -> g d
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