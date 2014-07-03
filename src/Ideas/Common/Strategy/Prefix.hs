-----------------------------------------------------------------------------
-- Copyright 2014, Open Universiteit Nederland. This file is distributed
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
--  $Id$

module Ideas.Common.Strategy.Prefix
   ( Prefix, emptyPrefix, makePrefix, showPrefix
   , prefixToSteps, lastStepInPrefix, activeLabels
   , searchModePrefix, prefixPath
   ) where

import Data.List
import Data.Maybe
import Control.Monad
import Ideas.Common.Classes
import Ideas.Common.Strategy.Abstract
import Ideas.Common.Strategy.Parsing
import Ideas.Common.Strategy.Sequence hiding (Step)
import Ideas.Common.Strategy.Path

-----------------------------------------------------------
--- Prefixes

-- | Abstract data type for a (labeled) strategy with a prefix (a sequence of
-- executed rules). A prefix is still "aware" of the labels that appear in the
-- strategy. A prefix is encoded as a list of integers (and can be reconstructed
-- from such a list: see @makePrefix@). The list is stored in reversed order.
newtype Prefix a = P { toState :: ParseState LabelInfo a }

instance Minor (Prefix a) where
   setMinor _ = id
   isMinor    = isMinor . toState

instance Firsts Prefix where
   ready    = ready . toState
   firsts x = [ (a, P y) | (a, y) <- firsts (toState x) ]
   
showPrefix :: Prefix a -> String
showPrefix = show . getPath . toState

-- | Construct the empty prefix for a labeled strategy
emptyPrefix :: LabeledStrategy a -> a -> Prefix a
emptyPrefix a = fromMaybe (error "emptyPrefix") . makePrefix emptyPath a

-- | Construct a prefix for a given list of integers and a labeled strategy.
makePrefix :: Monad m => Path -> LabeledStrategy a -> a -> m (Prefix a)
makePrefix path s = liftM P . replay path (mkCore s)
 where
   mkCore = processLabelInfo id . toCore . toStrategy

searchModePrefix :: (Step LabelInfo a -> Step LabelInfo a -> Bool) -> Prefix a -> Prefix a
searchModePrefix eq = P . searchModeState eq . toState

prefixToSteps :: Prefix a -> [Step LabelInfo a]
prefixToSteps = reverse . trace . toState

-- | Returns the last rule of a prefix (if such a rule exists)
lastStepInPrefix :: Prefix a -> Maybe (Step LabelInfo a)
lastStepInPrefix = listToMaybe . trace . toState

-- | Calculate the active labels
activeLabels :: Prefix a -> [LabelInfo]
activeLabels p = nub [l | Enter l <- steps] \\ [l | Exit l <- steps]
   where
      steps = prefixToSteps p
      
prefixPath :: Prefix a -> Path
prefixPath = getPath . toState