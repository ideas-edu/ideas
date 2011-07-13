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
module Domain.Math.Expr.Views 
   ( module Domain.Math.Expr.Views
   , (.+.), (.-.), neg, (.*.), (./.)
   ) where

import Common.Algebra.CoField
import Prelude hiding (recip, (^))
import Common.Library
import Domain.Math.Expr.Data
import Domain.Math.Expr.Symbols
import qualified Data.Set as S

------------------------------------------------------------
-- Smart constructors

infixr 8 .^.

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
   matchPlus (a :+: b)  = Just (a, b)
   matchPlus (a :-: b)  = Just (a, neg b)
   matchPlus (Negate a) = do (x, y) <- matchPlus a
                             Just (neg x, neg y)
   matchPlus _          = Nothing

timesView :: View Expr (Expr, Expr)
timesView = makeView matchTimes (uncurry (.*.))
 where
   matchTimes (a :*: b)  = Just (a, b)
   matchTimes (Negate a) = do (x, y) <- matchTimes a
                              Just (neg x, y)
   matchTimes _          = Nothing

divView :: View Expr (Expr, Expr)
divView = makeView matchDiv (uncurry (./.))
 where
   matchDiv (a :/: b)  = Just (a, b)
   matchDiv (Negate a) = do (x, y) <- matchDiv a
                            Just (neg x, y)
   matchDiv _          = Nothing

-------------------------------------------------------------
-- Sums and products

sumView :: Isomorphism Expr [Expr]
sumView = describe "View an expression as the sum of a list of elements, \
   \taking into account associativity of plus, its unit element zero, and \
   \inverse (both unary negation, and binary subtraction)." $
   "math.sum" @> sumEP
 where
   sumEP = (($ []) . f False) <-> (foldl (.+.) 0)

   f n (a :+: b)  = f n a . f n b
   f n (a :-: b)  = f n a . f (not n) b
   f n (Negate a) = f (not n) a
   f _ (Nat 0)    = id
   f n e          = if n then (neg e:) else (e:)

-- no distribution
simpleSumView :: Isomorphism Expr [Expr]
simpleSumView = sumEP
 where
   sumEP = (($ []) . f) <-> (foldl (.+) 0)

   f (a :+: b)           = f a . f b
   f (a :-: b)           = f a . f (-b)
   f (Nat 0)             = id
   f (Negate (Nat 0))    = id
   f (Negate (Negate a)) = f a
   f a                   = (a:)
   
   Nat 0 .+ b = b
   a .+ Nat 0 = a
   a .+ Negate b  = a :-: b
   a .+ b = a :+: b

productView :: Isomorphism Expr (Bool, [Expr])
productView = "math.product" @> productEP
 where
   productEP = (second ($ []) . f False) <-> g

   f r (a :*: b)  = f r a .&&. f r b
   f r (a :/: b)  = case a of -- two special cases (for efficiency)
                       Nat 1          -> f (not r) b
                       Negate (Nat 1) -> first not (f (not r) b)
                       _              -> f r a .&&. f (not r) b
   f r (Negate a) = first not (f r a)
   f r e          = (False, if r then (recip e:) else (e:))
   
   (n1, g1) .&&. (n2, g2) = (n1 /= n2, g1 . g2)
   
   g (b, xs) = (if b then neg else id) (foldl (.*.) 1 xs)
   
simpleProductView :: Isomorphism Expr (Bool, [Expr])
simpleProductView = "math.product.simple" @> simpleProductEP
 where
   simpleProductEP = (second ($ []) . f) <-> to productView

   f (a :*: b)  = f a .&&. f b
   f (Nat 1)    = (False, id)
   f (Negate a) = first not (f a)
   f e          = (False, (e:))
   
   (n1, g1) .&&. (n2, g2) = (n1 /= n2, g1 . g2)
   
-- helper to determine the name of the variable (move to a different module?)
selectVar :: Expr -> Maybe String
selectVar = f  . S.toList . varSet
 where
   f []  = Just "x" -- exceptional case (e.g., for constants)
   f [a] = Just a
   f _   = Nothing