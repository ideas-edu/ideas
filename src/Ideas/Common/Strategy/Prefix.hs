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
     Prefix, noPrefix, makePrefix, firstsOrdered
   , Core, runCore, replayCore
   , isEmptyPrefix, majorPrefix, searchModePrefix, prefixPaths
     -- * Path
   , Path, emptyPath, readPath, readPaths
   ) where

import Control.Monad
import Data.List (intercalate)
import Ideas.Common.Classes
import Ideas.Common.Id
import Ideas.Common.Rule
import Ideas.Common.Strategy.Choice
import Ideas.Common.CyclicTree
import Ideas.Common.Strategy.Process
import Ideas.Common.Strategy.Def
import Ideas.Common.Strategy.Sequence
import Ideas.Common.Strategy.Step
import Ideas.Common.Utils (splitsWithElem, readM)

--------------------------------------------------------------------------------
-- Prefix datatype

data Prefix a = Prefix
   { getPaths  :: [Path]
   , remainder :: Menu (Step a) (a, Prefix a)
   }

instance Show (Prefix a) where
   show = intercalate ";" . map show . prefixPaths

instance Monoid (Prefix a) where
   mempty = noPrefix
   mappend (Prefix xs p) (Prefix ys q) = Prefix (xs ++ ys) (p <|> q)

instance Firsts (Prefix a) where
   type Elem (Prefix a) = (Step a, a)

   ready  = hasDone . remainder
   firsts = map reorder . bests . remainder
    
firstsOrdered :: (Step a -> Step a -> Ordering) -> Prefix a -> [((Step a, a), Prefix a)]
firstsOrdered cmp = map reorder . bestsOrdered (cmp) . remainder

reorder :: (a, (b, c)) -> ((a, b), c)
reorder (x, (y, z)) = ((x, y), z)

--------------------------------------------------------------------------------
-- Running Core strategies

type Core a = CyclicTree Def (Rule a)

runCore :: Core a -> a -> [a]
runCore = runProcess . coreToProcess

coreToProcess :: Core a -> Process (Step a)
coreToProcess = fromBuilder . foldUnwind emptyAlg 
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
replayCore (Path is) = replay [] is . coreToProcess
 where
   replay acc []     p = (reverse acc, createPrefix p)
   replay acc (n:ns) p =
      case getByIndex n (menu p) of
         Just (a, r) -> replay (a:acc) ns r
         _ -> ([], const noPrefix)
         
   createPrefix p = Prefix [Path is] . flip (rec []) p

   rec ns a = cut . onMenuWithIndex f doneMenu . menu
    where
      f n st p =
         case st of
            RuleStep _ r -> choice
               [ RuleStep env r ?~> (b, mk b)
               | (b, env) <- transApply (transformation r) a
               ]
            _ -> st ?~> (a, mk a)
       where
         ms   = n:ns
         path = Path (is ++ reverse ms)
         mk b = Prefix [path] (rec ms b p)

         x ?~> y
            | isMinor st && stopped (snd y) = empty
            | otherwise = x |-> y

stopped :: Prefix a -> Bool
stopped = isEmpty . remainder

--------------------------------------------------------------------------------
-- Prefix fuctions

isEmptyPrefix :: Prefix a -> Bool
isEmptyPrefix = all (== emptyPath) . getPaths

-- | Transforms the prefix such that only major steps are kept in the remaining
-- strategy.
majorPrefix :: Prefix a -> Prefix a
majorPrefix prfx = prfx { remainder = onMenu f doneMenu (remainder prfx) }
 where
   f st (a, p)
      | isMajor st = st |-> (a, majorPrefix p)
      | otherwise  = remainder (majorPrefix p)

-- | The searchModePrefix transformation changes the process in such a way that
--   all intermediate states can only be reached by one path. A prerequisite is
--   that symbols are unique (or only used once).
searchModePrefix :: (Step a -> Step a -> Bool) -> Prefix a -> Prefix a
searchModePrefix eq prfx =
   prfx { remainder = rec (remainder (majorPrefix prfx)) }
 where
   rec m | hasDone m = doneMenu
         | otherwise = process (bests m)

   process [] = empty
   process ((st, (a, pr)):xs) =
      (st |-> (a, pr { remainder = rec (remainder pr) })) 
      <|> process (concatMap (change st) xs)

   change y (st, pair) =
      let f x = not (eq x y)
      in bests (filterPrefix f st pair)

filterPrefix :: (Step a -> Bool) -> Step a -> (a, Prefix a) -> Menu (Step a) (a, Prefix a)
filterPrefix cond = f
 where
   rec = onMenu f doneMenu
   f st (a, pr) = if cond st then st |-> (a, pr { remainder = rec (remainder pr) }) else empty

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