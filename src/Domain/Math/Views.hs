module Domain.Math.Views 
   ( module Domain.Math.Views
   , module Common.View -- export all view-related functions
   ) where

import Prelude hiding (recip)
import Common.View
import Domain.Math.Expr
import Domain.Math.Equation
import Control.Monad
import Data.List (nub)

------------------------------------------------------------
-- Smart constructors

(.+.) :: Expr -> Expr -> Expr
Nat 0 .+. b        = b
a     .+. Nat 0    = a
a     .+. Negate b = a .-. b
a     .+. b        = a :+: b

(.-.) :: Expr -> Expr -> Expr
Nat 0 .-. b        = neg b
a     .-. Nat 0    = a
a     .-. Negate b = a .+. b
a     .-. b        = a :-: b

neg :: Expr -> Expr
neg (Negate a) = a
neg a          = Negate a

(.*.) :: Expr -> Expr -> Expr
Nat 0    .*. _        = Nat 0
_        .*. Nat 0    = Nat 0
Nat 1    .*. b        = b
a        .*. Nat 1    = a
Negate a .*. b        = neg (a .*. b)
a        .*. Negate b = neg (a .*. b)
a        .*. b        = a :*: b

(./.) :: Expr -> Expr -> Expr
a ./. Nat 1           = a
Negate a ./. b        = neg (a ./. b)
a        ./. Negate b = neg (a ./. b)
a ./. b               = a :/: b

recip :: Expr -> Expr
recip (Nat 1 :/: a) = a
recip a             = Nat 1 :/: a

------------------------------------------------------------
-- Views of binary constructors

plusView :: View Expr (Expr, Expr)
plusView = makeView matchPlus (uncurry (.+.))
 where
   matchPlus :: Match Expr (Expr, Expr)
   matchPlus (a :+: b)  = Just (a, b)
   matchPlus (a :-: b)  = Just (a, neg b)
   matchPlus (Negate a) = do (x, y) <- matchPlus a
                             Just (neg x, neg y)
   matchPlus _          = Nothing

divView :: View Expr (Expr, Expr)
divView = makeView matchDiv (uncurry (./.))
 where
   matchDiv :: Match Expr (Expr, Expr)
   matchDiv (a :/: b)  = Just (a, b)
   matchDiv (Negate a) = do (x, y) <- matchDiv a
                            Just (neg x, y)
   matchDiv _          = Nothing

------------------------------------------------------------
-- Some constant views

conView :: View Expr Integer
conView = makeView f fromInteger
 where
   f (Nat n)    = return n
   f (Negate e) = fmap negate (f e)
   f _          = Nothing

fractionView :: View Expr (Integer, Integer) -- second component is positive
fractionView = divView >>> signs >>> (conView *** conView)
 where
   signs = makeView (Just . f) id
   f (a, Negate b) = f (neg a, b)
   f (a, b)        = (a, b)

rationalView :: View Expr Rational
rationalView = makeView exprToFractional fromRational

integerView :: View Expr Integer
integerView = makeView exprToNum fromIntegral
 
-------------------------------------------------------------
-- Sums and products

sumView :: View Expr [Expr]
sumView = makeView (return . ($ []) . f False) (foldl (.+.) 0)
 where
   f n (a :+: b)  = f n a . f n b
   f n (a :-: b)  = f n a . f (not n) b
   f n (Negate a) = f (not n) a
   f n e          = if n then (neg e:) else (e:)

productView :: View Expr (Bool, [Expr])
productView = makeView (Just . second ($ []) . f False) g
 where
   f r (a :*: b)  = f r a &&& f r b
   f r (a :/: b)  = f r a &&& f (not r) b
   f r (Negate a) = first not (f r a)
   f r e          = (False, if r then (recip e:) else (e:))
   
   (n1, g1) &&& (n2, g2) = (n1 /= n2, g1 . g2)
   
   g (b, xs) = (if b then neg else id) (foldl (.*.) 1 xs)

-------------------------------------------------------------
-- Equations

linearView :: View Expr (Rational, String, Rational)
linearView = makeView matchLin g
 where
   matchLin e = do
      (a, b) <- f e
      case nub (collectVars e) of
         [v] | a /= 0 -> Just (a, v, b)
         _            -> Nothing
 
   f (Var _)    = Just (1, 0)
   f (Nat n)    = Just (0, fromIntegral n)
   f (a :+: b)  = liftM2 (\(u,v) (w, x) -> (u+w, v+x)) (f a) (f b)
   f (a :-: b)  = f (a :+: Negate b)
   f (Negate a) = liftM (\(u,v) -> (-u,-v)) (f a)
   f (a :*: b)  = liftM2 (\(u,v) r -> (u*r,v*r)) (f a) (match rationalView b)
                     `mplus`
                  liftM2 (\r (u,v) -> (u*r,v*r)) (match rationalView a) (f b)
   f (a :/: b)  = do (u, v) <- f a 
                     r <- match rationalView b 
                     guard (r /= 0)
                     return (u/r,v/r)
   f _          = Nothing
   
   g (a, x, b) = (fromRational a .*. Var x) .+. fromRational b

equationView :: View (Equation Expr) (String, Rational)
equationView = makeView f g
 where
   f (lhs :==: rhs) = do 
      (a, x, b) <- match linearView (lhs - rhs)
      return (x, -b/a)
   g (x, r) = Var x :==: fromRational r