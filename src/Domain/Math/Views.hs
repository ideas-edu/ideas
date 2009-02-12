module Domain.Math.Views 
   ( module Domain.Math.Views
   , module Common.View -- export all view-related functions
   ) where

import Common.Utils (distinct)
import Common.View
import Domain.Math.Expr
import Domain.Math.Symbolic
import Control.Monad
import Data.List (nub)

------------------------------------------------------------
-- Views

plusView :: View Expr (Expr, Expr)
plusView = makeView f (uncurry (+))
 where
   f (a :+: b) = return (a, b)
   f (a :-: b) = return (a, negate b)
   f _         = Nothing

integerView :: View Expr Integer
integerView = makeView f fromInteger
 where
   f (Nat n)    = return n
   f (Negate e) = fmap negate (f e)
   f _          = Nothing

fractionView :: View Expr (Integer, Integer) -- second component is positive
fractionView = makeView f g
 where
   f (a :/: b)  = do 
      x <- match integerView a
      y <- match integerView b
      case compare y 0 of
         LT -> return (negate x, abs y)
         EQ -> fail "division by zero"
         GT -> return (x, y)
   f (Negate e) = fmap (\(x, y) -> (negate x, y)) (f e)
   f _          = Nothing
   
   g (x, y) = build integerView x / build integerView y
   
-------------------------------------------------------------
-- Views that originated from (linear) equations domain

sumView :: View Expr [Expr]
sumView = makeView (return . ($ []) . f False) g 
 where
   f n (a :+: b)  = f n a . f n b
   f n (a :-: b)  = f n a . f (not n) b
   f n (Negate a) = f (not n) a
   f n (Nat 0)    = id
   f n e          = if n then (negate e:) else (e:)
   
   g xs
      | null xs   = 0
      | otherwise = foldl1 op xs
    where 
       op a (Negate b) = a - b
       op a b          = a + b

productView :: View Expr [Expr]
productView = makeView (return . ($ []) . f) g 
 where
   f (a :*: b)  = f a . f b
   f (Negate a) = f a
   f (Nat 1)    = \xs -> if null xs then [] else negate (head xs) : tail xs
   f e          = (e:)
   
   g xs 
      | null xs   = 1
      | otherwise = foldl1 (*) xs

rationalView :: View Expr Rational
rationalView = makeView f fromRational
 where
   f e = do
      n <- match integerView e
      return (fromIntegral n)
    `mplus` do
      (a, b) <- match fractionView e
      guard (b /= 0)
      return (fromIntegral a / fromIntegral b)