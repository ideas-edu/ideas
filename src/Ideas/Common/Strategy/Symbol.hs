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
-- This module defines special symbols for labeling and atomicity.
--
-----------------------------------------------------------------------------

module Ideas.Common.Strategy.Symbol
   ( AtomicSymbol(..), LabelSymbol(..)
   , enterRule, exitRule, isEnterRule, isExitRule
   ) where

import Control.Monad
import Data.List
import Data.Maybe
import Ideas.Common.Id
import Ideas.Common.Rule

--------------------------------------------------------------------------------
-- Step

class Eq a => AtomicSymbol a where
   atomicOpen, atomicClose :: a

instance AtomicSymbol (Rule a) where
   atomicOpen  = idRule "atomic.open"
   atomicClose = idRule "atomic.close"

class Eq a => LabelSymbol a where
   isEnterSymbol :: a -> Bool

instance LabelSymbol (Rule a) where
   isEnterSymbol = isJust . isEnterRule

enterRule :: Id -> Rule a
enterRule l = idRule (l # "enter")

exitRule :: Id -> Rule a
exitRule l = idRule (l # "exit")

isEnterRule :: Rule a -> Maybe Id
isEnterRule st = do
   let n = getId st
   guard (unqualified n == "enter")
   return (initId n)

isExitRule :: Rule a -> Maybe Id
isExitRule st = do
   let n = getId st
   guard (unqualified n == "exit")
   return (initId n)

initId :: Id -> Id
initId = newId . intercalate "." . qualifiers