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
-----------------------------------------------------------------------------

module Ideas.Common.Strategy.CyclicTree
   ( -- * Data type
     CyclicTree
     -- * Constructor functions
   , node, node0, node1, node2, leaf, label
     -- * Querying
   , isNode, isLeaf, isLabel
     -- * Replace functions
   , replaceNode, replaceLeaf, replaceLabel, shrinkTree
     -- * Fold and algebra
   , fold, foldUnwind
   , CyclicTreeAlg, fNode, fLeaf, fLabel, fRec, fVar
   , emptyAlg, monoidAlg
   ) where

import Control.Monad
import Data.List (intercalate)
import Ideas.Common.Classes
import Ideas.Common.Id
import Test.QuickCheck hiding (label)

--------------------------------------------------------------
-- Data type

data CyclicTree a b
   = Node a [CyclicTree a b]
   | Leaf b
   | Label Id (CyclicTree a b)
   | Rec Int (CyclicTree a b)
   | Var Int

instance (Show a, Show b) => Show (CyclicTree a b) where
   show = fold Alg
      { fNode  = \a xs -> show a ++ par xs
      , fLeaf  = show
      , fLabel = \l s -> show l ++ ":" ++ s
      , fRec   = \n s -> '#' : show n ++ "=" ++ s
      , fVar   = \n   -> '#' : show n
      }

instance BiFunctor CyclicTree where
   biMap f g = fold idAlg {fNode = Node . f, fLeaf = Leaf . g}

instance Functor (CyclicTree d) where
   fmap = mapSecond

instance Applicative (CyclicTree d) where
   pure    = leaf
   p <*> q = fold idAlg {fLeaf = (<$> q)} p

instance Monad (CyclicTree d) where
   (>>=) = flip replaceLeaf

instance Foldable (CyclicTree d) where
   foldMap f = fold monoidAlg {fLeaf = f}

instance Traversable (CyclicTree d) where
   traverse f = fold emptyAlg
      { fNode  = \a -> fmap (node a) . sequenceA
      , fLeaf  = fmap leaf . f
      , fLabel = fmap . label
      , fRec   = fmap . Rec
      , fVar   = pure . Var
      }

instance Fix (CyclicTree a b) where
   fix f = Rec n (f (Var n))
    where
      vs = vars (f (Var (-1)))
      n  = maximum (-1 : vs) + 1

instance (Arbitrary a, Arbitrary b) => Arbitrary (CyclicTree a b) where
   arbitrary = sized arbTree
   shrink    = shrinkTree

arbTree :: (Arbitrary a, Arbitrary b) => Int -> Gen (CyclicTree a b)
arbTree = rec 0
 where
   rec vi 0 = frequency $
      (3, leaf <$> arbitrary)
      : [ (1, elements (map Var [1..vi])) | vi > 0 ]
   rec vi n = frequency
      [ (3, node <$> arbitrary <*> ms)
      , (2, rec vi 0)
      , (1, label <$> genId <*> m)
      , (1, Rec (vi+1) <$> rec (vi+1) (n `div` 2))
      ]
    where
      m = rec vi (n `div` 2)
      genId = elements [ newId [c] | c <- ['A' .. 'Z']]
      ms = choose (0, 3) >>= \i -> replicateM i m

shrinkTree :: CyclicTree a b -> [CyclicTree a b]
shrinkTree tree =
   case tree of
      Node a ts -> ts ++ map (node a) (shrinkTrees ts)
      Label l t -> t : map (Label l) (shrinkTree t)
      Rec n t   -> map (Rec n) (shrinkTree t)
      _ -> []

-- shrink exactly one tree
shrinkTrees :: [CyclicTree a b] -> [[CyclicTree a b]]
shrinkTrees []    = []
shrinkTrees (t:ts) = map (:ts) (shrinkTree t) ++ map (t:) (shrinkTrees ts)

-- local helpers
par :: [String] -> String
par xs | null xs   = ""
       | otherwise = "(" ++ intercalate ", " xs ++ ")"

vars :: CyclicTree a b -> [Int]
vars = fold monoidAlg {fVar = return}

--------------------------------------------------------------
-- Constructor functions

node :: a -> [CyclicTree a b] -> CyclicTree a b
node = Node

node0 :: a -> CyclicTree a b
node0 a = node a []

node1 :: a -> CyclicTree a b -> CyclicTree a b
node1 a x = node a [x]

node2 :: a -> CyclicTree a b -> CyclicTree a b -> CyclicTree a b
node2 a x y = node a [x, y]

leaf :: b -> CyclicTree a b
leaf = Leaf

label :: IsId n => n -> CyclicTree a b -> CyclicTree a b
label = Label . newId

--------------------------------------------------------------
-- Querying

isNode :: CyclicTree a b -> Maybe (a, [CyclicTree a b])
isNode (Node a xs) = Just (a, xs)
isNode _ = Nothing

isLeaf :: CyclicTree a b -> Maybe b
isLeaf (Leaf b) = Just b
isLeaf _ = Nothing

isLabel :: CyclicTree a b -> Maybe (Id, CyclicTree a b)
isLabel (Label l t) = Just (l, t)
isLabel _ = Nothing

--------------------------------------------------------------
-- Replace functions

replaceNode :: (a -> [CyclicTree a b] -> CyclicTree a b) -> CyclicTree a b -> CyclicTree a b
replaceNode f = fold idAlg {fNode = f}

replaceLabel :: (Id -> CyclicTree a b -> CyclicTree a b) -> CyclicTree a b -> CyclicTree a b
replaceLabel f = fold idAlg {fLabel = f}

replaceLeaf :: (b -> CyclicTree a c) -> CyclicTree a b -> CyclicTree a c
replaceLeaf f = fold idAlg {fLeaf = f}

--------------------------------------------------------------
-- Fold and algebra

fold :: CyclicTreeAlg a b t -> CyclicTree a b -> t
fold alg = rec
 where
   rec (Node a ts) = fNode alg a (map rec ts)
   rec (Leaf b)    = fLeaf alg b
   rec (Label l t) = fLabel alg l (rec t)
   rec (Rec n t)   = fRec alg n (rec t)
   rec (Var n)     = fVar alg n

foldUnwind :: CyclicTreeAlg a b t -> CyclicTree a b -> t
foldUnwind alg = start . fold Alg
   { fNode  = \a fs sub -> fNode alg a (map ($ sub) fs)
   , fLeaf  = \b _      -> fLeaf alg b
   , fLabel = \l f sub  -> fLabel alg l (f sub)
   , fRec   = \n f sub  -> let this = f (extend n this sub)
                           in this
   , fVar   = \n sub    -> sub n
   }
 where
   start f = f (error "foldUnwind: unbound var")
   extend n a sub i
      | i == n    = a
      | otherwise = sub i

data CyclicTreeAlg a b t = Alg
   { fNode  :: a -> [t] -> t
   , fLeaf  :: b -> t
   , fLabel :: Id -> t -> t
   , fRec   :: Int -> t -> t
   , fVar   :: Int -> t
   }

idAlg :: CyclicTreeAlg a b (CyclicTree a b)
idAlg = Alg Node Leaf Label Rec Var

emptyAlg :: CyclicTreeAlg a b t
emptyAlg = let f = error "emptyAlg: uninitialized" in Alg f f f f f

monoidAlg :: Monoid m => CyclicTreeAlg a b m
monoidAlg = Alg (const mconcat) mempty (const id) (const id) mempty