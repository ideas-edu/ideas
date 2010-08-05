-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
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

import Common.Classes
import Common.Utils
import Common.Strategy.Abstract
import Common.Strategy.Grammar
import Common.Transformation
import Common.Derivation
import Common.Strategy.Location
import Data.Maybe

-----------------------------------------------------------
--- Prefixes

-- | Abstract data type for a (labeled) strategy with a prefix (a sequence of 
-- executed rules). A prefix is still "aware" of the labels that appear in the 
-- strategy. A prefix is encoded as a list of integers (and can be reconstructed 
-- from such a list: see @makePrefix@). The list is stored in reversed order.
data Prefix a = P [(Int, PStep a)] (DerivationTree (PStep a) ())

type PStep = Step (StrategyLocation, LabelInfo)

instance Show (Prefix a) where
   show (P xs _) = show (reverse (map fst xs))

instance Eq (Prefix a) where
   P xs _ == P ys _ = map fst xs == map fst ys

-- | Construct the empty prefix for a labeled strategy
emptyPrefix :: LabeledStrategy a -> Prefix a
emptyPrefix = fromMaybe (error "emptyPrefix") . makePrefix []

-- | Construct a prefix for a given list of integers and a labeled strategy.
makePrefix :: Monad m => [Int] -> LabeledStrategy a -> m (Prefix a)
makePrefix is ls = rec [] is start
 where
   mkCore = processLabelInfo snd . addLocation . toCore . toStrategy
   start  = makeTree (mkCore ls)
 
   rec acc [] t = return (P acc t)
   rec acc (n:ns) t =
      case drop n (branches t) of
         (step, st):_ -> rec ((n, step):acc) ns st
         _            -> fail ("invalid prefix: " ++ show is)

instance Apply Prefix where
   applyAll p = results . prefixTree p

-- | Create a derivation tree with a "prefix" as annotation.
prefixTree :: Prefix a -> a -> DerivationTree (Prefix a) a
prefixTree (P xs t) = changeLabel snd . runTree (decorate xs t)

decorate :: [(Int, PStep a)] -> DerivationTree (PStep a) () -> DerivationTree (PStep a) (Prefix a)
decorate xs t =
   let list = zipWith make [0..] (branches t)
       make i (s, st) = (s, decorate ((i,s):xs) st)
   in addBranches list (singleNode (P xs t) (endpoint t))
 
-- | Returns the steps that belong to the prefix
prefixToSteps :: Prefix a -> [PStep a]
prefixToSteps (P xs _) = [ step | (_, step) <- reverse xs ]
 
-- | Retrieves the rules from a list of steps
stepsToRules :: [Step l a] -> [Rule a]
stepsToRules steps = [ r | RuleStep _ r <- steps ]

-- | Returns the last rule of a prefix (if such a rule exists)
lastStepInPrefix :: Prefix a -> Maybe (PStep a)
lastStepInPrefix (P xs _) = safeHead [ step | (_, step) <- xs ]

-------------------------------------------------------------------
-- Copied from core

runTree :: Apply f => DerivationTree (f a) info -> a -> DerivationTree (f a, info) a
runTree t a = addBranches list (singleNode a (endpoint t))
 where
   list = concatMap make (branches t)
   make (f, st) = [ ((f, root st), runTree st b) | b <- applyAll f a ]