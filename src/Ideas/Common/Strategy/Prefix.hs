{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- Copyright 2015, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Basic machinery for fully executing a strategy expression, or only
-- partially. Partial execution results in a prefix that keeps the current
-- locations in the strategy (a list of @Path@s) for continuing the execution
-- later on. A path can be used to reconstruct the sequence of steps already
-- performed (a so-called trace). Prefixes can be merged with the Monoid
-- operation.
--
-----------------------------------------------------------------------------

module Ideas.Common.Strategy.Prefix
   ( -- * Prefix
     Prefix, noPrefix, makePrefix, firstsOrdered
   , replayProcess
   , isEmptyPrefix, majorPrefix, searchModePrefix, prefixPaths
     -- * Path
   , Path, emptyPath, readPath, readPaths
   ) where

import Control.Monad
import Data.List (intercalate)
import Ideas.Common.Classes
import Ideas.Common.Environment
import Ideas.Common.Rule
import Ideas.Common.Strategy.Choice
import Ideas.Common.Strategy.Process
import Ideas.Common.Strategy.Sequence
import Ideas.Common.Utils (splitsWithElem, readM)

--------------------------------------------------------------------------------
-- Prefix datatype

data Prefix a = Prefix
   { getPaths  :: [Path]
   , remainder :: Menu (Rule a) (a, Environment, Prefix a)
   }

instance Show (Prefix a) where
   show = intercalate ";" . map show . prefixPaths

instance Monoid (Prefix a) where
   mempty = noPrefix
   mappend (Prefix xs p) (Prefix ys q) = Prefix (xs ++ ys) (p .|. q)

instance Firsts (Prefix a) where
   type Elem (Prefix a) = (Rule a, a, Environment)

   ready  = hasDone . remainder
   firsts = map reorder . bests . remainder

firstsOrdered :: (Rule a -> Rule a -> Ordering) -> Prefix a -> [((Rule a, a, Environment), Prefix a)]
firstsOrdered cmp = map reorder . bestsOrdered cmp . remainder

reorder :: (a, (b, env, c)) -> ((a, b, env), c)
reorder (x, (y, env, z)) = ((x, y, env), z)

--------------------------------------------------------------------------------
-- Constructing a prefix

-- | The error prefix (i.e., without a location in the strategy).
noPrefix :: Prefix a
noPrefix = Prefix [] empty

-- | Make a prefix from a core strategy and a start term.
makePrefix :: Process (Rule a) -> a -> Prefix a
makePrefix = snd . replayProcess emptyPath

-- | Construct a prefix by replaying a path in a core strategy: the third
-- argument is the current term.
replayProcess :: Path -> Process (Rule a) -> ([Rule a], a -> Prefix a)
replayProcess (Path is) = replay [] is
 where
   replay acc []     p = (reverse acc, createPrefix p)
   replay acc (n:ns) p =
      case getByIndex n (menu p) of
         Just (a, r) -> replay (a:acc) ns r
         _ -> ([], const noPrefix)

   createPrefix p = Prefix [Path is] . flip (rec []) p

   rec ns a = cut . onMenuWithIndex f doneMenu . menu
    where
      f n r p = choice
         [ r ?~> (b, env, mk b)
         | (b, env) <- transApply (transformation r) a
         ]
       where
         ms   = n:ns
         path = Path (is ++ reverse ms)
         mk b = Prefix [path] (rec ms b p)

         x ?~> y@(_, _, q)
            | isMinor r && stopped q = empty
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
   f r (a, env, p)
      | isMajor r = r |-> (a, env, majorPrefix p)
      | otherwise = remainder (majorPrefix p)

-- | The searchModePrefix transformation changes the process in such a way that
--   all intermediate states can only be reached by one path. A prerequisite is
--   that symbols are unique (or only used once).
searchModePrefix :: Prefix a -> Prefix a
searchModePrefix prfx =
   prfx { remainder = rec (remainder (majorPrefix prfx)) }
 where
   rec m | hasDone m = doneMenu
         | otherwise = process (bests m)

   process [] = empty
   process ((r, (a, env, pr)):xs) =
      (r |-> (a, env, pr { remainder = rec (remainder pr) }))
      .|. process (concatMap (change r) xs)

   change y (r, pair) =
      bests (filterPrefix (/= y) r pair)

filterPrefix :: (Rule a -> Bool) -> Rule a -> (a, Environment, Prefix a) -> Menu (Rule a) (a, Environment, Prefix a)
filterPrefix cond = f
 where
   rec = onMenu f doneMenu
   f r (a, env, pr) = if cond r then r |-> (a, env, pr { remainder = rec (remainder pr) }) else empty

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
