module Domain.Math.View.Basic
   ( module Domain.Math.View.Basic
   , module Domain.Math.View.Numeric
   , module Common.View -- export all view-related functions
   ) where

import Prelude hiding (recip, (^))
import Common.View
import Domain.Math.Expr
import Domain.Math.View.Numeric
import Domain.Math.Expr.Symbols
import Domain.Math.Data.Equation
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
neg (Nat 0)    = 0
neg (Negate a) = a
neg (a :+: b)  = neg a .-. b
neg (a :-: b)  = neg a .+. b
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

(.^.) :: Expr -> Expr -> Expr
Nat 0 .^. _ = Nat 0
Nat 1 .^. _ = Nat 1
_ .^. Nat 0 = Nat 1
a .^. Nat 1 = a
a .^. b     = a ^ b

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

timesView :: View Expr (Expr, Expr)
timesView = makeView matchTimes (uncurry (.*.))
 where
   matchTimes :: Match Expr (Expr, Expr)
   matchTimes (a :*: b)  = Just (a, b)
   matchTimes (Negate a) = do (x, y) <- matchTimes a
                              Just (neg x, y)
   matchTimes _          = Nothing

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

-----------------------------------------------------------
-- Views for powers

-- a*x^b, e.g., -3*x^2, but also 3*x, and x
-- This view also merges powers in multiplications
powerView :: View Expr (Expr, String, Integer)
powerView = makeView matchPower buildPower
 where
   matchPower e = do
      let vs = nub (collectVars e)
      guard (length vs == 1)
      (b, xs) <- match productView e
      let op (a, x, n) y =
             case y of 
                Var _                  -> return (a,x,n+1)
                Sym s [Var _, Nat m] | s == powerSymbol -> 
                   return (a,x,n+m)
                Nat 1 :/: b | noVars y -> return (a ./. b, x, n) -- Not nice! instead, xs should be normalized
                _ | noVars y           -> return (a .*. y, x, n)
                _ -> Nothing 
      foldM op (if b then -1 else 1, head vs, 0) xs 
      
   buildPower (a, x, n) = a .*. (Var x .^. fromInteger n)
{-
-- This view also merges equivalent power factors
polynomialView :: View Expr (String, IM.IntMap Expr)
polynomialView = makeView matchPolynomial buildPolynomial
 where
   matchPolynomial e = do
      xs <- match sumView e
      ts <- flip mapM xs $ \x ->
               fmap Left  (match powerView x) `mplus` 
               fmap Right (match rationalView x)
      let vs = nub [ x | Left (_, x, _) <- ts ]
      guard (length vs == 1)
      let op (Left (e, _, n)) im = IM.insertWith (.+.) (fromIntegral n) e im
          op (Right r )       im = IM.insertWith (.+.) 0 (fromRational r) im
      return (head vs, foldr op IM.empty ts)
   
   buildPolynomial (x, im) = build sumView (map f (reverse (IM.toList im)))
    where f (n, a) = a .*. (Var x .^. fromIntegral n)
   
-- a*x^2 + b*x + c

quadraticView :: View Expr (String, Rational, Rational, Rational)
quadraticView = polynomialView >>> makeView matchQ buildQ 
 where
   matchQ (x, im) = do
      let keys = IM.keysSet im
          f n  = IM.findWithDefault 0 n im
      guard (IS.findMin keys == 0 && IS.findMax keys == 2 && IS.size keys == 3)
      [a, b, c] <- mapM (match rationalView . f) [2,1,0]
      return (x, a, b, c)
   buildQ (x, a, b, c) = (x, IM.fromList [(2, fromRational a), (1, fromRational b), (0, fromRational c)])
-}
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