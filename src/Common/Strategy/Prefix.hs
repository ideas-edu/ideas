-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
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
   ( Prefix, emptyPrefix, makePrefix
   , prefixToSteps, prefixTree, stepsToRules, lastStepInPrefix
   ) where

import Common.DerivationTree
import Common.Strategy.Abstract
import Common.Strategy.Parsing
import Common.Rule
import Control.Monad
import Data.Maybe

-----------------------------------------------------------
--- Prefixes

-- | Abstract data type for a (labeled) strategy with a prefix (a sequence of
-- executed rules). A prefix is still "aware" of the labels that appear in the
-- strategy. A prefix is encoded as a list of integers (and can be reconstructed
-- from such a list: see @makePrefix@). The list is stored in reversed order.
data Prefix a = P (State LabelInfo a)

prefixPair :: Prefix a -> (Int, [Bool])
prefixPair (P s) = (length (trace s), reverse (choices s))

prefixIntList :: Prefix a -> [Int]
prefixIntList = f . prefixPair
 where
   f (0, []) = []
   f (n, bs) = n : map (\b -> if b then 0 else 1) bs

instance Show (Prefix a) where
   show = show . prefixIntList

instance Eq (Prefix a) where
   a == b = prefixPair a == prefixPair b

-- | Construct the empty prefix for a labeled strategy
emptyPrefix :: LabeledStrategy a -> Prefix a
emptyPrefix = fromMaybe (error "emptyPrefix") . makePrefix []

-- | Construct a prefix for a given list of integers and a labeled strategy.
makePrefix :: Monad m => [Int] -> LabeledStrategy a -> m (Prefix a)
makePrefix []     ls = makePrefix [0] ls
makePrefix (i:is) ls = liftM P $
   replay i (map (==0) is) (mkCore ls)
 where
   mkCore = processLabelInfo id . toCore . toStrategy

-- | Create a derivation tree with a "prefix" as annotation.
prefixTree :: Bool -> Prefix a -> a -> DerivationTree (Prefix a) a
prefixTree search (P s) a = fmap value $ updateAnnotations (\_ _ -> P) $
   parseDerivationTree search s {value = a}

prefixToSteps :: Prefix a -> [Step LabelInfo a]
prefixToSteps (P t) = reverse (trace t)

-- | Retrieves the rules from a list of steps
stepsToRules :: [Step l a] -> [Rule a]
stepsToRules xs = [ r | RuleStep _ r <- xs ]

-- | Returns the last rule of a prefix (if such a rule exists)
lastStepInPrefix :: Prefix a -> Maybe (Step LabelInfo a)
lastStepInPrefix (P t) = listToMaybe (trace t)