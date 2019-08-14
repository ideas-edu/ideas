-----------------------------------------------------------------------------
-- Copyright 2019, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Datatype for representing derivations as a tree. The datatype stores all
-- intermediate results as well as annotations for the steps.
--
-----------------------------------------------------------------------------

module Ideas.Common.DerivationTree
   ( -- * Data types
     DerivationTree
     -- * Constructors
   , singleNode, addBranches, makeTree
     -- * Query
   , root, endpoint, branches, subtrees
   , leafs, lengthMax
     -- * Adapters
   , restrictHeight, restrictWidth, updateAnnotations
   , cutOnStep, mergeMaybeSteps, sortTree, cutOnTerm
     -- * Conversions
   , derivation, randomDerivation, derivations
   ) where

import Control.Arrow
import Control.Monad
import Data.List
import Data.Maybe
import Ideas.Common.Classes
import Ideas.Common.Derivation
import System.Random

-----------------------------------------------------------------------------
-- Data type definitions for derivation trees and derivation lists

data DerivationTree s a = DT
   { root     :: a                           -- ^ The root of the tree
   , endpoint :: Bool                        -- ^ Is this node an endpoint?
   , branches :: [(s, DerivationTree s a)]   -- ^ All branches
   }
 deriving Show

instance Functor (DerivationTree s) where
   fmap = mapSecond

instance BiFunctor DerivationTree where
   biMap f g (DT a b xs) = DT (g a) b (map (biMap f (biMap f g)) xs)

-----------------------------------------------------------------------------
-- Constructors for a derivation tree

-- | Constructs a node without branches; the boolean indicates whether the
-- node is an endpoint or not
singleNode :: a -> Bool -> DerivationTree s a
singleNode a b = DT a b []

-- | Branches are attached after the existing ones (order matters)
addBranches :: [(s, DerivationTree s a)] -> DerivationTree s a -> DerivationTree s a
addBranches new (DT a b xs) = DT a b (xs ++ new)

makeTree :: (a -> (Bool, [(s, a)])) -> a -> DerivationTree s a
makeTree f = rec
 where
   rec a = let (b, xs) = f a
           in addBranches (map (mapSecond rec) xs) (singleNode a b)

-----------------------------------------------------------------------------
-- Inspecting a derivation tree

-- | Returns the annotations at a given node
annotations :: DerivationTree s a -> [s]
annotations = map fst . branches

-- | Returns all subtrees at a given node
subtrees :: DerivationTree s a -> [DerivationTree s a]
subtrees = map snd . branches

-- | Returns all leafs, i.e., final results in derivation. Be careful:
-- the returned list may be very long
leafs :: DerivationTree s a -> [a]
leafs t = [ root t | endpoint t ] ++ concatMap leafs (subtrees t)

-- | The argument supplied is the maximum number of steps; if more steps are
-- needed, Nothing is returned
lengthMax :: Int -> DerivationTree s a -> Maybe Int
lengthMax n = join . fmap (f . derivationLength) . derivation
            . commit . restrictHeight (n+1)
 where
    f i = if i<=n then Just i else Nothing

updateAnnotations :: (a -> s -> a -> t) -> DerivationTree s a -> DerivationTree t a
updateAnnotations f = rec
 where
   rec (DT a b xs) =
      let g (s, t) = (f a s (root t), rec t)
      in DT a b (map g xs)

-----------------------------------------------------------------------------
-- Changing a derivation tree

-- | Restrict the height of the tree (by cutting off branches at a certain depth).
-- Nodes at this particular depth are turned into endpoints
restrictHeight :: Int -> DerivationTree s a -> DerivationTree s a
restrictHeight n t
   | n == 0    = singleNode (root t) True
   | otherwise = t {branches = map f (branches t)}
 where
   f = mapSecond (restrictHeight (n-1))

-- | Restrict the width of the tree (by cutting off branches).
restrictWidth :: Int -> DerivationTree s a -> DerivationTree s a
restrictWidth n = rec
 where
   rec t = t {branches = map (mapSecond rec) (take n (branches t))}

-- | Commit to the left-most derivation (even if this path is unsuccessful)
commit :: DerivationTree s a -> DerivationTree s a
commit = restrictWidth 1

-- | Filter out intermediate steps, and merge its branches (and endpoints) with
-- the rest of the derivation tree
mergeSteps :: (s -> Bool) -> DerivationTree s a -> DerivationTree s a
mergeSteps p = rec
 where
   rec t = addBranches (concat list) (singleNode (root t) isEnd)
    where
      new = map rec (subtrees t)
      (bools, list) = unzip (zipWith f (annotations t) new)
      isEnd = endpoint t || or bools
      f s st
         | p s       = (False, [(s, st)])
         | otherwise = (endpoint st, branches st)

sortTree :: (l -> l -> Ordering) -> DerivationTree l a -> DerivationTree l a
sortTree f t = t {branches = change (branches t) }
 where
   change = map (mapSecond (sortTree f)) . sortBy cmp
   cmp (l1, _) (l2, _) = f l1 l2

mergeMaybeSteps :: DerivationTree (Maybe s) a -> DerivationTree s a
mergeMaybeSteps = mapFirst fromJust . mergeSteps isJust

cutOnStep :: (s -> Bool) -> DerivationTree s a -> DerivationTree s a
cutOnStep p = rec
 where
   rec t = t {branches = map f (branches t)}
   f (s, t)
      | p s       = (s, singleNode (root t) True)
      | otherwise = (s, rec t)

cutOnTerm :: (a -> Bool) -> DerivationTree s a -> DerivationTree s a
cutOnTerm p (DT r e bs) =
    DT r e (map (second (cutOnTerm p)) $ filter (not . p . root . snd) bs)

-----------------------------------------------------------------------------
-- Conversions from a derivation tree

-- | All possible derivations (returned in a list)
derivations :: DerivationTree s a -> [Derivation s a]
derivations t =
   [ emptyDerivation (root t) | endpoint t ] ++
   [ (root t, r) `prepend` d | (r, st) <- branches t, d <- derivations st ]

-- | The first derivation (if any)
derivation :: DerivationTree s a -> Maybe (Derivation s a)
derivation = listToMaybe . derivations

-- | Return  a random derivation (if any exists at all)
randomDerivation :: RandomGen g => g -> DerivationTree s a -> Maybe (Derivation s a)
randomDerivation g t = msum xs
 where
   (xs, g0) = shuffle g list
   list     = [ Just (emptyDerivation (root t)) | endpoint t ] ++
              map make (branches t)
   make (r, st) = do
      d <- randomDerivation g0 st
      return ((root t, r) `prepend` d)

shuffle :: RandomGen g => g -> [a] -> ([a], g)
shuffle g0 xs = rec g0 [] (length xs) xs
 where
   rec g acc n ys =
      case splitAt i ys of
         (as, b:bs) -> rec g1 (b:acc) (n-1) (as++bs)
         _ -> (acc, g)
    where
      (i, g1) = randomR (0, n-1) g