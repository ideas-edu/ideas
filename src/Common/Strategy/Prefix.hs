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
-- A prefix encodes a sequence of steps already performed (a so-called trace), 
-- and allows to continue the derivation at that particular point.
--
-----------------------------------------------------------------------------
module Common.Strategy.Prefix 
   ( Prefix, emptyPrefix, makePrefix, Step(..)
   , prefixToSteps, prefixTree, stepsToRules, lastStepInPrefix
   ) where

import Common.Apply
import Common.Utils
import Common.Strategy.Abstract
import Common.Strategy.Core
import Common.Transformation
import Common.Derivation
import qualified Common.Strategy.Grammar as Grammar
import Common.Strategy.Location

-----------------------------------------------------------
--- Prefixes

-- | Abstract data type for a (labeled) strategy with a prefix (a sequence of 
-- executed rules). A prefix is still "aware" of the labels that appear in the 
-- strategy. A prefix is encoded as a list of integers (and can be reconstructed 
-- from such a list: see @makePrefix@). The list is stored in reversed order.
data Prefix a = P [(Int, Step a)] (Grammar.Grammar (Step a))

instance Show (Prefix a) where
   show (P xs _) = show (reverse (map fst xs))

instance Eq (Prefix a) where
   P xs _ == P ys _ = map fst xs == map fst ys

-- | Construct the empty prefix for a labeled strategy
emptyPrefix :: LabeledStrategy a -> Prefix a
emptyPrefix = makePrefix []

-- | Construct a prefix for a given list of integers and a labeled strategy.
makePrefix :: [Int] -> LabeledStrategy a -> Prefix a
makePrefix is ls = rec [] is start
 where
   start = withSteps ls
   
   rec acc [] g = P acc g
   rec acc (n:ns) g = 
      case drop n (Grammar.firsts g) of
         (z, h):_ -> rec ((n, z):acc) ns h
         _        -> P [] start

-- | The @Step@ data type can be used to inspect the structure of the strategy
data Step a = Begin StrategyLocation 
            | Step (Rule a) 
            | End StrategyLocation
   deriving (Show, Eq)

instance Apply Step where
   applyAll (Step r)  = applyAll r
   applyAll (Begin _) = return
   applyAll (End _)   = return

instance Apply Prefix where
   applyAll p = results . prefixTree p

-- | Create a derivation tree with a "prefix" as annotation.
prefixTree :: Prefix a -> a -> DerivationTree (Prefix a) a
prefixTree (P xs g) a =
   addBranches list (singleNode a (Grammar.empty g))
 where
   add (i, (step, rest)) = P ((i, step):xs) rest
   list = [ (newPrefix, prefixTree newPrefix b)
          | triple@(_, (step, _)) <- zip [0..] (Grammar.firsts g)
          , let newPrefix = add triple
          , b <- applyAll step a
          ]
 
-- | Returns the steps that belong to the prefix
prefixToSteps :: Prefix a -> [Step a]
prefixToSteps (P xs _) = map snd (reverse xs)
 
-- | Retrieves the rules from a list of steps
stepsToRules :: [Step a] -> [Rule a]
stepsToRules steps = [ r | Step r <- steps ]

-- | Returns the last rule of a prefix (if such a rule exists)
lastStepInPrefix :: Prefix a -> Maybe (Step a)
lastStepInPrefix (P xs _) = safeHead (map snd xs)

-- local helper function
withSteps :: LabeledStrategy a -> Grammar.Grammar (Step a)
withSteps s = toStepGrammar (toCore (unlabel s))

toStepGrammar :: Core String a -> Grammar.Grammar (Step a)
toStepGrammar = toGrammar (forLabel, forRule) . addLocation . labelMajorRules show
 where
   forLabel (loc, _) g = Grammar.symbol (Begin loc) Grammar.<*> g Grammar.<*> Grammar.symbol (End loc)
   forRule r = Grammar.symbol (Step r)
   
labelMajorRules :: (Rule a -> l) -> Core l a -> Core l a
labelMajorRules f = mapCore Label g
 where
   g r | isMajorRule r = Label (f r) (Rule r)
       | otherwise     = Rule r