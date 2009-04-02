{-# LANGUAGE FlexibleContexts #-}
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
module Domain.Programming.ContractM
   ( -- * Combintors and help functions
     prop, fun, pair, list, (<>), (>->), (>>->), run
     -- * Standard contracts
   , nat, nonempty, true, false, is, preserves
   ) where

import Control.Monad.Instances
import Data.List
import Domain.Programming.Blame

-----------------------------------------------------------------------------
-- | Type synonym for functions with location information
type (:->) a b = Locs -> a -> b
infixr :->

-----------------------------------------------------------------------------
-- | Contract combinators
prop :: Monad m => (a -> Bool) -> a :-> m a
prop p ls a = if p a then return a else fail ("contract failed: " ++ blame ls)

fun :: Monad m => (a :-> m b) -> (b -> c :-> m d) -> Locs -> (b -> c) -> a :-> m d
fun pre post lsf f ls x = pre (lsf +> ls) x >>= (\x' -> ((post x' lsf) . f) x')

pair :: Monad m => (a :-> m b) -> (b -> c :-> m d) -> (a, c) :-> m (b, d)
pair c1 c2 ls (a, b) = c1 ls a >>= (\a' -> (c2 a' ls) b >>= (\b' -> return (a', b')))

list :: Monad m => (a :-> m b) -> [a] :-> m [b]
list c ls = mapM (c ls)
 
(<>) :: Monad m => (b :-> m c) -> (a :-> m b) -> a :-> m c
c1 <> c2  = \ls a -> c2 ls a >>= c1 ls

infixr 4 >->
pre >->  post = fun pre (const post)

infixr 4 >>->
pre >>-> post = fun pre post

infixl 3 <>

run :: (a :-> Maybe b) -> Locs -> a -> b
run f ls a = case f ls a of
               Just b -> b
               Nothing -> error "assertion error"

-----------------------------------------------------------------------------
-- | Basic contracts
true, false :: Monad m => a :-> m a
true = prop (const True) 
false = prop (const False)
nat :: Monad m => Int :-> m Int
nat = prop (>=0)
nonempty :: Monad m => [a] :-> m [a]
nonempty = prop (not . null)

preserves :: (Monad m, Eq b) => (a -> b) -> Locs -> (a -> a) -> a :-> m a
preserves f = true >>-> (\x-> (prop (\y -> f x == f y)))

is ::(Monad m, Eq b) => (a -> b) -> Locs -> (a -> b) -> a :-> m b
is f =  true >>-> (\x-> (prop (\y -> y == f x)))

-----------------------------------------------------------------------------
-- | Help functions
l0 = makeloc (Def "")

-----------------------------------------------------------------------------
-- | Contracted functions

insertionSort :: Ord a => [a] -> [a]
insertionSort = foldr insert [] 

insert' :: (Monad ((->) [a]), Monad m, Ord a) => a :-> [a] :-> m [a]
insert' = (true >-> ord >-> ord) l0 insert

ord :: (Monad m, Ord a) => [a] :-> m [a]
ord = prop (\x -> ordered x)

ordered                 ::  Ord a => [a] -> Bool
ordered []              =   True
ordered [a]             =   True
ordered (a1 : a2 : as)  =   a1 <= a2 && ordered (a2 : as)

insertionSort' :: (Monad m, Ord a) => [a] :-> m [a]
insertionSort' = (true >-> ord) l0 insertionSort

head' :: Monad m => [a] :-> m a
head' = (nonempty >-> true) l0 head
