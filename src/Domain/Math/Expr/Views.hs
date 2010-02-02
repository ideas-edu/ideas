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
module Domain.Math.Expr.Views where

import Prelude hiding (recip, (^))
import Common.View
import Domain.Math.Expr.Data
import Domain.Math.Expr.Symbols
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
a        .*. (Nat 1 :/: b) = a ./. b
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

simpleProductView :: View Expr (Bool, [Expr])
simpleProductView = makeView (Just . second ($ []) . f) g
 where
   f (a :*: b)  = f a &&& f b
   f (Negate a) = first not (f a)
   f e          = (False, (e:))
   
   (n1, g1) &&& (n2, g2) = (n1 /= n2, g1 . g2)
   
   g (b, xs) = (if b then neg else id) (foldl (.*.) 1 xs)

productView :: View Expr (Bool, [Expr])
productView = makeView (Just . second ($ []) . f False) g
 where
   f r (a :*: b)  = f r a &&& f r b
   f r (a :/: b)  = case a of -- two special cases (for efficiency)
                       Nat 1          -> f (not r) b
                       Negate (Nat 1) -> first not (f (not r) b)
                       _              -> f r a &&& f (not r) b
   f r (Negate a) = first not (f r a)
   f r e          = (False, if r then (recip e:) else (e:))
   
   (n1, g1) &&& (n2, g2) = (n1 /= n2, g1 . g2)
   
   g (b, xs) = (if b then neg else id) (foldl (.*.) 1 xs)
   
-- helper to determine the name of the variable (move to a different module?)
selectVar :: Expr -> Maybe String
selectVar = f . nub . collectVars
 where
   f []  = Just "x" -- exceptional case (e.g., for constants)
   f [a] = Just a
   f _   = Nothing