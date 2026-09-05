{-# LANGUAGE TypeFamilies #-}
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

import Data.Char
import Data.List (intercalate)
import Data.Maybe
import Data.Semigroup as Sem
import Ideas.Common.Classes
import Ideas.Common.Environment
import Ideas.Common.Rewriting.Term
import Ideas.Common.Rule
import Ideas.Common.Strategy.Choice
import Ideas.Common.Strategy.Process
import Ideas.Common.Strategy.Sequence
import Ideas.Common.Strategy.StrategyTree
import Ideas.Utils.Prelude (splitsWithElem, readM)

--------------------------------------------------------------------------------
-- Prefix datatype

data Prefix a = Prefix
   { getPaths  :: [Path]
   , remainder :: Menu (Rule a) (a, Environment, Prefix a)
   }

instance Show (Prefix a) where
   show = intercalate ";" . map show . prefixPaths

instance Sem.Semigroup (Prefix a) where
   (Prefix xs p) <> (Prefix ys q) = Prefix (xs ++ ys) (p .|. q)

instance Monoid (Prefix a) where
   mempty  = noPrefix
   mappend = (<>)

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
makePrefix :: Process (Leaf a) -> a -> Prefix a
makePrefix = snd . replayProcess emptyPath

-- | Construct a prefix by replaying a path in a core strategy: the third
-- argument is the current term.
replayProcess :: Path -> Process (Leaf a) -> ([Rule a], a -> Prefix a)
replayProcess (Path is) = fromMaybe ([], const noPrefix) . replay [] is
 where
   replay acc path p =
      case path of
         []         -> return (reverse acc, createPrefix p)
         Input _:_  -> Nothing
         Index n:ns -> do
            (leaf, q) <- getByIndex n (menu p)
            case (leaf, ns) of
               (LeafRule r, _) -> replay (r:acc) ns q
               (LeafDyn d, Input t:ns2) -> do
                  a <- dynamicFromTerm d t
                  replay acc ns2 (treeToProcess a .*. q)
               _ -> Nothing

   createPrefix p = Prefix [Path is] . flip (rec []) p

   rec ns a = cut . onMenuWithIndex f doneMenu . menu
    where
      f n (LeafDyn d) p = fromMaybe empty $ do
         t <- dynamicToTerm d a
         s <- dynamicFromTerm d t
         return (rec (Input t:Index n:ns) a (treeToProcess s .*. p))
      f n (LeafRule r) p = choice
         [ r ?~> (b, env, mk b)
         | (b, env) <- transApply (transformation r) a
         ]
       where
         ms   = Index n:ns
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
-- of integers and terms (the latter act as input for the dynamic strategies).
newtype Path = Path [PathItem]
   deriving Eq

data PathItem = Index Int | Input Term
   deriving Eq

instance Show PathItem where
   show (Index n) = show n
   show (Input t) = show t

instance Read PathItem where
   readsPrec n s =
      case dropWhile isSpace s of
         s2@(c:_) | isDigit c -> map (mapFirst Index) (readsPrec n s2)
         s2 -> map (mapFirst Input) (readsPrec n s2)

instance Show Path where
   show (Path is) = show is
   showList = (++) . intercalate ";" . map show

-- | The empty path.
emptyPath :: Path
emptyPath = Path []

readPath :: String -> Maybe Path
readPath = fmap Path . readM

readPaths :: String -> Maybe [Path]
readPaths = mapM readPath . splitsWithElem ';'