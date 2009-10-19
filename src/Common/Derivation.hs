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
-- Datatype for representing derivations as a tree. The datatype stores all 
-- intermediate results as well as annotations for the steps.
--
-----------------------------------------------------------------------------
module Common.Derivation 
   ( -- * Data types 
     DerivationTree, Derivations, Derivation
     -- * Constructors
   , singleNode, addBranch, addBranches
     -- * Query
   , root, endpoint, branches, annotations, subtrees, steps, stepsMax
     -- * Adapters
   , restrictHeight, mergeSteps, commit
     -- * Conversions
   , derivation, derivations
   ) where

import Common.Utils (safeHead)
import Control.Arrow
import Control.Monad
import Data.Maybe

-----------------------------------------------------------------------------
-- Data type definitions for derivation trees and derivation lists

data DerivationTree s a = DT 
   { root     :: a                           -- ^ The root of the tree
   , endpoint :: Bool                        -- ^ Is this node an endpoint?
   , branches :: [(s, DerivationTree s a)]   -- ^ All branches
   }
 deriving Show

type Derivations s a = [Derivation s a]

data Derivation s a = D a [(s, a)]

instance (Show s, Show a) => Show (Derivation s a) where
   show (D a xs) = unlines $
      show a : concatMap (\(r, b) -> ["   => " ++ show r, show b]) xs

instance Functor (DerivationTree s) where
   fmap f (DT a b xs) = DT (f a) b (map (second (fmap f)) xs)

instance Functor (Derivation s) where
   fmap f (D a xs) = D (f a) (map (second f) xs)

-----------------------------------------------------------------------------
-- Constructors for a derivation tree

-- | Constructs a node without branches; the boolean indicates whether the 
-- node is an endpoint or not
singleNode :: a -> Bool -> DerivationTree s a
singleNode a b = DT a b []

-- | Add a single branch
addBranch :: (s, DerivationTree s a) -> DerivationTree s a -> DerivationTree s a
addBranch = addBranches . return

-- | Branches are attached after the existing ones (order matters)
addBranches :: [(s, DerivationTree s a)] -> DerivationTree s a -> DerivationTree s a
addBranches new (DT a b xs) = DT a b (xs ++ new)

-----------------------------------------------------------------------------
-- Inspecting a derivation tree

-- | Returns the annotations at a given node
annotations :: DerivationTree s a -> [s]
annotations = map fst . branches

-- | Returns all subtrees at a given node
subtrees :: DerivationTree s a -> [DerivationTree s a]
subtrees = map snd . branches

-- | Returns all final terms
results :: DerivationTree s a -> [a]
results = map f . derivations
 where f (D a xs) = last (a:map snd xs)

-- | Returns the number of steps for the first derivation
steps :: DerivationTree s a -> Maybe Int
steps = fmap f . derivation
 where f (D _ xs) = length xs

-- | The argument supplied is the maximum number of steps; if more steps are
-- needed, Nothing is returned
stepsMax :: Int -> DerivationTree s a -> Maybe Int
stepsMax n = join . fmap f . steps . commit . restrictHeight (n+1)
 where f i = if i<=n then Just i else Nothing

-----------------------------------------------------------------------------
-- Changing a derivation tree

-- Restrict the height of the tree (by cutting off branches at a certain depth).
-- Nodes at this particular depth are turned into endpoints
restrictHeight :: Int -> DerivationTree s a -> DerivationTree s a
restrictHeight n t
   | n == 0    = t {endpoint = True, branches = []}
   | otherwise = t {branches = map f (branches t)} 
 where
   f = second (restrictHeight (n-1))

-- Filter out intermediate steps, and merge its branches (and endpoints) with
-- the rest of the derivation tree
mergeSteps :: (s -> Bool) -> DerivationTree s a -> DerivationTree s a
mergeSteps p = rec 
 where
   rec (DT a b xs) = 
      DT a (or (b:map (endpoint . snd) xs)) (concatMap f xs)
   f (r, d)
      | p r       = [(r, rec d)]
      | otherwise = branches (rec d)

-- Commit to the left-most derivation (even if this path is unsuccessful)
commit :: DerivationTree s a -> DerivationTree s a
commit t = t {branches = map (second commit) (take 1 (branches t))}

-----------------------------------------------------------------------------
-- Conversions from a derivation tree

-- | All possible derivations (returned in a list)
derivations :: DerivationTree s a -> Derivations s a
derivations t = map (D (root t)) $
   [ [] | endpoint t ] ++
   [ ((r,a2):ys) | (r, st) <- branches t, D a2 ys <- derivations st ]

-- | The first derivation (if any)
derivation :: DerivationTree s a -> Maybe (Derivation s a)
derivation = safeHead . derivations