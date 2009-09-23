module Domain.Math.Numeric.Views
   ( integralView, realView
   , integerView, rationalView, doubleView
   , integerNormalForm, rationalNormalForm, rationalRelaxedForm, fractionForm
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
           return (x Prelude.^ numerator ry)
   f _ _ = Nothing
   
integerView :: View Expr Integer
integerView = integralView

rationalView :: View Expr Rational
rationalView = makeView (match realView) fromRational

-- No floating view
doubleView :: View Expr Double
doubleView = makeView (exprToNum doubleSym)
                      (fromRational . flip approxRational 0.0001)
 
-------------------------------------------------------------------
-- Numeric views in normal form 

-- N or -N (where n is a natural number)
integerNormalForm :: View Expr Integer
integerNormalForm = makeView (optionNegate f) fromInteger
 where
   f (Nat n) = Just n
   f _       = Nothing

rationalNormalForm :: View Expr Rational
rationalNormalForm = makeView (optionNegate f) fromRational
 where   
   f (Nat a :/: Nat b) = do
      guard (a > 0 && b > 1 && gcd a b == 1)
      Just (fromInteger a / fromInteger b)
   f (Nat n) = Just (fromInteger n)
   f _       = Nothing

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
         a :+: b  -> liftM2 (+)    (rec a) (rec b)
         a :*: b  -> liftM2 (*)    (rec a) (rec b)
         a :-: b  -> liftM2 (-)    (rec a) (rec b)
         Negate a -> liftM  negate (rec a)
         Nat n    -> return (fromInteger n)
         a :/: b  -> do x <- rec a; y <- rec b; f divideSymbol [x, y]
         Sqrt a   -> do x <- rec a; f rootSymbol [x, 2]
         Var _    -> fail "exprToNum: variable"
         Sym s xs -> mapM rec xs >>= f s

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