{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  alex.gerdes@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- A module with typed, higher-order, and first-class contracts, from
-- Hinze et al. (http://people.cs.uu.nl/johanj/publications/Contract.pdf).
--
-----------------------------------------------------------------------------
module Domain.Programming.Contract 
   ( -- * Abstract data type
     Contract (..)
     -- * Assertion function
   , assert
     -- * Standard contracts
   , nat, nonempty
   ) where

import Data.List (intersperse)

----------------------------------------------------------
-- Abstract data type

-- | (Generalised) Abstract data type for a contract: 
data Contract a where
  Prop     :: (a -> Bool) -> Contract a
  Function :: Contract a -> (a -> Contract b) -> Contract (a :->> b)
  Pair     :: Contract a -> (a -> Contract b) -> Contract (a, b)
  List     :: Contract a -> Contract [a]
  And      :: Contract a -> Contract a -> Contract a

-- | newtype
newtype (:->>) a b = Fun {app :: Locs -> a -> b}
infixr :->>

type Loc = Int
data Locs = NegPos {neg :: [Loc], pos :: [Loc]}

-- | is it your fault ;-)
blame :: Locs -> String
blame ls =  "the expression " ++ show (head (pos ls)) ++ " is to blame" 
         ++ (case tail (pos ls) of 
               []  -> " . "
               ls' -> " (the violation was cause by the expression(s) " ++
                      concat (intersperse ", " (map show ls')) ++ ").")

l :: Loc -> Locs
l l = NegPos [] [l]

(|>) :: Locs -> Locs -> Locs
NegPos ns ps |> NegPos ns' ps' = NegPos (ps ++ ns') (ns ++ ps') 

-- | The assert function
assert :: Contract a -> (a :->> a)
assert (Prop p)         = prop p
assert (Function c1 c2) = fun (assert c1) (assert . c2)
assert (Pair c1 c2)     = pair (assert c1) (assert . c2)
assert (List c)         = list (assert c)
assert (And c1 c2)      = assert c2 <> assert c1

prop :: (a -> Bool) -> (a :->> a)
prop p = Fun (\ls a -> if p a then a else error ("contract failed: " ++ blame ls))

fun :: (a1 :->> b1) -> (b1 -> a2 :->> b2) -> ((b1 :->> a2):->> (a1 :->> b2))
fun g h = Fun (\lsf f -> Fun (\lx ->
            (\x' -> (app (h x') lsf . app f lx) x') . app g (lsf |> lx)))

pair :: (a1 :->> b1) -> (b1 -> a2 :->> b2) -> ((a1, a2) :->> (b1, b2))
pair g h = Fun (\ls (a1, a2) -> (\a1' -> (a1', app (h a1') ls a2)) (app g ls a1))

list :: (a :->> b) -> ([a] :->> [b])
list g = Fun (\ls -> map (app g ls))

(<>) :: (b :->> c) -> (a :->> b) -> (a :->> c)
g <> h = Fun (\ls -> app g ls . app h ls)

--(->>) :: Contract a -> Contract b -> Contract (a :->> b)
c1 ->> c2 = Function c1 (const c2)

----------------------------------------------------------
-- Example contracts
nat :: Contract Int
nat = Prop (>=0)
true, false :: Contract a
true = Prop (const True)
false = Prop (const False)
nonempty :: Contract [a]
nonempty = Prop (not . null)
nat2nat :: Contract (Int :->> Int)
nat2nat = nat ->> nat
--isqrt = Function nat (\n->Prop (\r->r>=0 && r^2 <= n && n < (r+1)^2))


----------------------------------------------------------
-- Contracted functions
--head' :: [a] :->> a
head' = app (app (assert (nonempty ->> true)) (l 0) (nl head)) (l 0)

nl f = Fun (const f)

f c = app (assert c) (l 0)
t = app (app (assert nat2nat) (l 0) (nl (\x->1+x))) (l 0)
