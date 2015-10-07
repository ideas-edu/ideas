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
-- This module defines special symbols
--
-----------------------------------------------------------------------------
--  $Id: Core.hs 7590 2015-04-21 07:26:58Z bastiaan $

module Ideas.Common.Strategy.Step
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