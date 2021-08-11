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
-- * This module provides an interface to structure a collection of examples.
-- Examples can be taken from (lists of) concrete values, or from random
-- generators. Both types can be marked as test items. Examples can be assigned
-- a level of difficulty (ranging from very easy to very difficult). Test items
-- do not have a difficulty level. Examples can be grouped into sub-collections
-- and assigned an identifier. Use the @Monoid@ operations for combining
-- examples.
--
-----------------------------------------------------------------------------

module Ideas.Common.Examples
   ( -- * Examples type
     Examples
     -- * Constructing examples
   , example, exampleList, examplesFor, examplesWithDifficulty
   , random, group, forTesting
     -- * Assigning difficulty
   , difficulty, veryEasy, easy, medium, difficult, veryDifficult
     -- * Transformations and queries
   , isEmpty, size, flatten, groups
   , topLevelExamples, topLevelRandoms, topLevelTests, topLevelRandomTests
   , allExamples, allRandoms, allTests, allRandomTests
     -- * Difficulty type
   , Difficulty(..), readDifficulty
   ) where

import Data.Char
import Data.Maybe
import Ideas.Common.Id
import Test.QuickCheck

-----------------------------------------------------------------------------
-- Examples

data Examples a = Examples
   { groups :: [(Id, Examples a)] -- ^ Top-level groups
   , items  :: [Item a]
   }

instance Semigroup (Examples a) where
   xs <> ys = Examples (groups xs <> groups ys) (items xs <> items ys)

instance Monoid (Examples a) where
   mempty  = Examples [] []
   mappend = (<>)

instance Functor Examples where
   fmap f (Examples xs ys) = Examples [ (n, fmap f g) | (n, g) <- xs ] (map (fmap f) ys)

data Item a = Example (Maybe Difficulty) a
            | Random  (Maybe Difficulty) (Gen a)
            | Test a
            | RandomTest (Gen a)

instance Functor Item where
   fmap f (Example d a)  = Example d (f a)
   fmap f (Test a)       = Test (f a)
   fmap f (Random d g)   = Random d (fmap f g)
   fmap f (RandomTest g) = RandomTest (fmap f g)

-- | One example
example :: a -> Examples a
example = single . Example Nothing

-- | List of examples
exampleList :: [a] -> Examples a
exampleList = Examples [] . map (Example Nothing)

-- | List of examples with the same difficulty
examplesFor :: Difficulty -> [a] -> Examples a
examplesFor d = examplesWithDifficulty . zip (repeat d)

-- | List of examples with their own difficulty
examplesWithDifficulty :: [(Difficulty, a)] -> Examples a
examplesWithDifficulty = Examples [] . map (uncurry (Example . Just))

-- | Use a random generator (from QuickCheck) as example generator
random :: Gen a -> Examples a
random = single . Random Nothing

group :: Id -> Examples a -> Examples a
group n xs = Examples [(n, xs)] []

-- | Assign difficulty (to all items without a difficulty level)
difficulty :: Difficulty -> Examples a -> Examples a
difficulty d = changeItems f
 where
   f (Example Nothing a) = Example (Just d) a
   f (Random Nothing a)  = Random  (Just d) a
   f x = x

-- | Turn examples (and random generators) into tests (and test generators)
forTesting :: Examples a -> Examples a
forTesting = changeItems f
 where
   f (Example _ a) = Test a
   f (Random _ a)  = RandomTest a
   f x = x

-- Querying

-- | Top-level examples
topLevelExamples :: Examples a -> [(Maybe Difficulty, a)]
topLevelExamples = collectItems f
 where
   f (Example md a) = Just (md, a)
   f _ = Nothing

-- | Top-level random generators
topLevelRandoms :: Examples a -> [(Maybe Difficulty, Gen a)]
topLevelRandoms = collectItems f
 where
   f (Random md g) = Just (md, g)
   f _ = Nothing

-- | Top-level test cases
topLevelTests :: Examples a -> [a]
topLevelTests = collectItems f
 where
   f (Test a) = Just a
   f _ = Nothing

-- | Top-level test generators
topLevelRandomTests :: Examples a -> [Gen a]
topLevelRandomTests = collectItems f
 where
   f (RandomTest g) = Just g
   f _ = Nothing

-- | All examples (also in groups)
allExamples :: Examples a -> [(Maybe Difficulty, a)]
allExamples = topLevelExamples . flatten

-- | All random generators (also in groups)
allRandoms :: Examples a -> [(Maybe Difficulty, Gen a)]
allRandoms = topLevelRandoms . flatten

-- | All test cases (also in groups)
allTests :: Examples a -> [a]
allTests = topLevelTests . flatten

-- | All test generators (also in groups)
allRandomTests :: Examples a -> [Gen a]
allRandomTests = topLevelRandomTests . flatten

-- | Flatten examples into one collection without subgroups
flatten :: Examples a -> Examples a
flatten = Examples [] . getItems

-- | Number of examples, including those in subgroups
size :: Examples a -> Int
size = length . getItems

-- | Tests if there ar no examples
isEmpty :: Examples a -> Bool
isEmpty = null . getItems

-- local helpers
single :: Item a -> Examples a
single x = Examples [] [x]

getItems :: Examples a -> [Item a]
getItems xs = concatMap (getItems . snd) (groups xs) ++ items xs

changeItems :: (Item a -> Item a) -> Examples a -> Examples a
changeItems f = rec
 where
   rec xs = Examples (map g (groups xs)) (map f (items xs))
   g (n, ys) = (n, rec ys)

collectItems :: (Item a -> Maybe b) -> Examples a -> [b]
collectItems f = mapMaybe f . items

-----------------------------------------------------------------------------
-- Difficulty

data Difficulty = VeryEasy | Easy | Medium | Difficult | VeryDifficult
   deriving (Eq, Ord, Enum)

instance Show Difficulty where
   show = (xs !!) . fromEnum
    where
      xs = ["very_easy", "easy", "medium", "difficult", "very_difficult"]

instance Read Difficulty where
   readsPrec _ s =
      case concatMap f txt of
         "veryeasy"      -> [(VeryEasy, xs)]
         "easy"          -> [(Easy, xs)]
         "medium"        -> [(Medium, xs)]
         "difficult"     -> [(Difficult, xs)]
         "verydifficult" -> [(VeryDifficult, xs)]
         _               -> []
    where
      (txt, xs) = span p (dropWhile isSpace s)
      p c   = isAlpha c || c `elem` "_-"
      f c   = [toLower c | c `notElem` "_-"]

-- | Parser for difficulty levels, which ignores non-alpha charactes (including
-- spaces) and upper/lower case distinction.
readDifficulty :: String -> Maybe Difficulty
readDifficulty s =
   case filter p [VeryEasy .. VeryDifficult] of
            [a] -> Just a
            _   -> Nothing
 where
   normal = filter isAlpha . map toLower
   p = (== normal s) . normal . show

veryEasy, easy, medium, difficult, veryDifficult :: Examples a -> Examples a
veryEasy      = difficulty VeryEasy
easy          = difficulty Easy
medium        = difficulty Medium
difficult     = difficulty Difficult
veryDifficult = difficulty VeryDifficult