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
     Contract (..), (:->), Locs (..), Loc
     -- * Assertion function
   , assert, assert'
     -- * Combintors and help functions
   , (->>), (->>>), (&), l, l0, fun, app, app2, app3, liftC, liftC2, liftC3
     -- * Standard contracts
   , nat, nonempty, true, false
   ) where

import Prelude hiding (id)
import Data.List (intersperse, insert)

----------------------------------------------------------
-- Abstract data type

-- | (Generalised) Abstract data type for a contract: 
data Contract a where
  Prop     :: (a -> Bool) -> Contract a
  Function :: Contract a -> (a -> Contract b) -> Contract (a :-> b)
  Pair     :: Contract a -> (a -> Contract b) -> Contract (a, b)
  List     :: Contract a -> Contract [a]
  And      :: Contract a -> Contract a -> Contract a

-- | newtype
newtype (:->) a b = Fun {app :: Locs -> a -> b}
infixr :->

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

l0 = l 0

(|>) :: Locs -> Locs -> Locs
NegPos ns ps |> NegPos ns' ps' = NegPos (ps ++ ns') (ns ++ ps') 

-- | The assert function
assert' ::  Contract aT -> (Locs -> aT -> aT)
assert' (Prop p) locs a = if p a then a else error ("contract failed: " ++ blame locs)
assert'  (Function c1 c2) locsf  f =  Fun (\ locx -> (\ x' -> (assert' (c2 x') locsf . app f locx) x') . assert' c1 (locsf |> locx))
assert'  (Pair c1 c2)  locs   (a1, a2) = (\ a1' -> (a1' , assert' (c2 a1') locs a2)) (assert' c1 locs a1)
assert'  (List c)      locs   as  =   map (assert' c locs) as
assert'  (And c1 c2)   locs   a =   (assert' c2 locs . assert' c1 locs) a


assert :: Contract a -> (a :-> a)
assert (Prop p)         = prop p
assert (Function c1 c2) = function (assert c1) (assert . c2)
assert (Pair c1 c2)     = pair (assert c1) (assert . c2)
assert (List c)         = list (assert c)
assert (And c1 c2)      = assert c2 <> assert c1

prop :: (a -> Bool) -> (a :-> a)
prop p = Fun (\ls a -> if p a then a else error ("contract failed: " ++ blame ls))

function :: (a1 :-> b1) -> (b1 -> a2 :-> b2) -> ((b1 :-> a2) :-> (a1 :-> b2))
function g h = Fun (\lsf f -> Fun (\lx ->
            (\x' -> (app (h x') lsf . app f lx) x') . app g (lsf |> lx)))

pair :: (a1 :-> b1) -> (b1 -> a2 :-> b2) -> ((a1, a2) :-> (b1, b2))
pair g h = Fun (\ls (a1, a2) -> (\a1' -> (a1', app (h a1') ls a2)) (app g ls a1))

list :: (a :-> b) -> ([a] :-> [b])
list g = Fun (\ls -> map (app g ls))

(<>) :: (b :-> c) -> (a :-> b) -> (a :-> c)
g <> h = Fun (\ls -> app g ls . app h ls)

fun f = Fun (\_ x -> f x)
apply f loc x = app f (l loc) x
id = fun 

infixr 4 ->>
--(->>) :: Contract a -> Contract b -> Contract (a :->> b)
c1 ->> c2 = Function c1 (const c2)

infixr 4 ->>>
(->>>) = Function 

infixl 3 &
(&) = And

liftC :: (a -> b) -> (a :-> b)
liftC  f = fun f
liftC2 :: (a -> b -> c) -> (a :-> b :-> c)
liftC2 f = fun (\x -> fun (\y -> f x y))
liftC3 :: (a -> b -> c -> d) -> (a :-> b :-> c :-> d)
liftC3 f = fun (\x -> fun (\y -> fun (\z -> f x y z)))

app2 :: (a :-> b :-> c) -> Locs -> (a -> b -> c)
app2 f loc x y   = app (app f loc x) loc y
app3 :: (a :-> b :-> c :-> d) -> Locs -> (a -> b -> c -> d)
app3 f loc x y z = app (app (app f loc x) loc y) loc z

----------------------------------------------------------
-- Standard contracts
nat :: Contract Int
nat = Prop (>=0)
true, false :: Contract a
true = Prop (const True)
false = Prop (const False)
nonempty :: Contract [a]
nonempty = Prop (not . null)
nat2nat :: Contract (Int :-> Int)
nat2nat = nat ->> nat

preserves :: Eq b => (a -> b) -> Contract (a :-> a)
preserves f =  Function  true(\x-> (Prop(\  y ->  f x == f y)))

is :: Eq b => (a -> b) -> Contract (a :-> b)
is f = Function  true(\x-> (Prop(\  y ->  y == f x)))

----------------------------------------------------------
-- Contracted functions
head' :: [a] :-> a
head' = assert' (nonempty ->> true) l0 (liftC head)

app_head' :: [a] -> a
app_head' = app head' l0

ord'  ::  (Ord aT) => Contract [aT]
ord'  =   Prop(\  x ->  ordered x)

ordered                 ::  (Ord aT) => [aT] -> Bool
ordered []              =   True
ordered [a]             =   True
ordered (a1 : a2 : as)  =   a1 <= a2 && ordered (a2 : as)

insert' ::  (Ord aT) => aT :-> [aT] :-> [aT]
insert' = assert' (true ->> ord' ->> ord') l0 (liftC2 insert)

insertC :: (Ord a) => a -> [a] -> [a]
insertC = app2 insert' l0

