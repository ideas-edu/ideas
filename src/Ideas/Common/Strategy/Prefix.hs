{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- Copyright 2015, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Basic machinery for fully executing a core strategy expression, or only
-- partially. Partial execution results in a prefix that keeps the current
-- locations in the strategy (a list of @Path@s) for continuing the execution
-- later on. A path can be used to reconstruct the sequence of steps already
-- performed (a so-called trace). Prefixes can be merged with the Monoid
-- operation.
--
-----------------------------------------------------------------------------
--  $Id$

module Ideas.Common.Strategy.Prefix
   ( -- * Prefix
     Prefix, noPrefix, makePrefix, Core, runCore, replayCore
   , isEmptyPrefix, majorPrefix, searchModePrefix, prefixPaths
     -- * Path
   , Path, emptyPath, readPath, readPaths
   ) where

import Control.Monad
import Data.Function
import Data.List (intercalate)
import Ideas.Common.Classes
import Ideas.Common.Id
import Ideas.Common.Rule
import Ideas.Common.Strategy.Choice
import Ideas.Common.CyclicTree
import Ideas.Common.Strategy.Derived
import Ideas.Common.Strategy.Process
import Ideas.Common.Strategy.Def
import Ideas.Common.Strategy.Sequence
import Ideas.Common.Strategy.Step
import Ideas.Common.Utils (fst3, splitsWithElem, readM)

--------------------------------------------------------------------------------
-- Prefix datatype

data Prefix a = Prefix
   { getPaths  :: [Path]
   , remainder :: Process (Step a, a, Path)
   }

instance Show (Prefix a) where
   show = intercalate ";" . map show . prefixPaths

instance Monoid (Prefix a) where
   mempty = noPrefix
   mappend (Prefix xs p) (Prefix ys q) = Prefix (xs ++ ys) (p <|> q)

instance Firsts (Prefix a) where
   type Elem (Prefix a) = (Step a, a)

   menu = onMenu f doneMenu . menu . remainder
    where
      f (st, a, path) p = (st, a) |-> Prefix [path] p

--------------------------------------------------------------------------------
-- Running Core strategies

type Core a = CyclicTree Def (Rule a)

runCore :: Core a -> a -> [a]
runCore = runProcess . coreToProcess

coreToProcess :: Core a -> Process (Step a)
coreToProcess = toProcess . foldUnwind emptyAlg 
   { fNode  = useDef
   , fLeaf  = single . RuleStep mempty
   , fLabel = \l p -> Enter l ~> p <*> (Exit l ~> done)
   }
   
--------------------------------------------------------------------------------
-- Constructing a prefix

-- | The error prefix (i.e., without a location in the strategy).
noPrefix :: Prefix a
noPrefix = Prefix [] empty

-- | Make a prefix from a core strategy and a start term.
makePrefix :: Core a -> a -> Prefix a
makePrefix = snd . replayCore emptyPath

-- | Construct a prefix by replaying a path in a core strategy: the third
-- argument is the current term.
replayCore :: Path -> Core a -> ([Step a], a -> Prefix a)
replayCore path core =
   let (acc, p) = runPath path (withPath (coreToProcess core))
   in (map fst acc, Prefix [path] . applySteps p)

runPath :: Path -> Process a -> ([a], Process a)
runPath (Path is) = rec [] is
 where
   rec acc []     p = (reverse acc, p)
   rec acc (n:ns) p =
      case getByIndex n (menu p) of
         Just (a, r) -> rec (a:acc) ns r
         _ -> ([], empty)

applySteps :: Process (Step a, Path) -> a -> Process (Step a, a, Path)
applySteps p a0 = prune (isMajor . fst3) (scan f a0 p)
 where
   f a (RuleStep _ r, path) =
      [ (b, (RuleStep env r, b, path))
      | (b, env) <- transApply (transformation r) a
      ]
   f a (st, path) = [(a, (st, a, path))]

withPath :: Process a -> Process (a, Path)
withPath = rec []
 where
   rec ns = mapWithIndex (f ns) done . menu

   f ns n a p =
      let ms = n:ns
      in (a, Path (reverse ms)) ~> rec ms p
 
--------------------------------------------------------------------------------
-- Prefix fuctions

isEmptyPrefix :: Prefix a -> Bool
isEmptyPrefix = all (== emptyPath) . getPaths

-- | Transforms the prefix such that only major steps are kept in the remaining
-- strategy.
majorPrefix :: Prefix a -> Prefix a
majorPrefix prfx = prfx { remainder = hide (isMajor . fst3) (remainder prfx) }

-- | The searchModePrefix transformation changes the process in such a way that
--   all intermediate states can only be reached by one path. A prerequisite is
--   that symbols are unique (or only used once).
searchModePrefix :: (Step a -> Step a -> Bool) -> Prefix a -> Prefix a
searchModePrefix eq prfx =
   prfx { remainder = rec (remainder (majorPrefix prfx)) }
 where
   eq3 = eq `on` fst3

   rec p | ready p   = done
         | otherwise = process (firsts p)

   process [] = empty
   process ((a, p):xs) =
      let ys = map fst $ firsts (a ~> p)
      in (a ~> rec p) <|> process (concatMap (change ys) xs)

   change ys (a, q) =
      let f x = all (not . eq3 x) ys
      in firsts $ filterP f (a ~> q)

-- | Returns the current @Path@.
prefixPaths :: Prefix a -> [Path]
prefixPaths = getPaths

--------------------------------------------------------------------------------
-- Path

-- | A path encodes a location in a strategy. Paths are represented as a list
-- of integers.
newtype Path = Path [Int]
   deriving Eq

instance Show Path where
   show (Path is) = show is

-- | The empty path.
emptyPath :: Path
emptyPath = Path []

readPath :: Monad m => String -> m Path
readPath = liftM Path . readM

readPaths :: Monad m => String -> m [Path]
readPaths = mapM readPath . splitsWithElem ';'