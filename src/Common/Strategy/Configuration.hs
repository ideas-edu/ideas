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
-----------------------------------------------------------------------------
module Common.Strategy.Configuration 
   ( -- Types and constructors
     StrategyConfiguration, ConfigItem
   , ConfigLocation(..), ConfigAction(..)
     --  Configure
  ,  configure
     -- Combinators
   , hide, expose, fold, unfold, skip, insert
   ) where

import Common.Strategy.Abstract
import Common.Strategy.Core hiding (Skip)
import Common.Strategy.Location
import Common.Transformation

---------------------------------------------------------------------
-- Types and constructors

type StrategyConfiguration = [ConfigItem]
type ConfigItem = (ConfigLocation, ConfigAction)

data ConfigLocation
   = ByName     String
   | ByGroup    String
   | ByLocation StrategyLocation
 deriving Show
 
data ConfigAction = Hide | Expose | Fold | Unfold | Skip | Insert
   deriving Show

---------------------------------------------------------------------
-- Configure

configure :: StrategyConfiguration -> LabeledStrategy a -> LabeledStrategy a
configure cfg ls = 
   label (strategyName ls) (configureCore cfg (toCore (unlabel ls)))

configureCore :: StrategyConfiguration -> Core LabelInfo a -> Core LabelInfo a
configureCore cfg = mapCore f g . addLocation
 where
   f pair        a = Label (change pair []) a
   g (Just pair) r = Rule (Just (change pair (ruleGroups r))) r
   g Nothing     r = Rule Nothing r
   
   change pair@(_, info) groups = 
      let actions = getActions pair groups cfg
      in foldr doAction info actions
   
getActions :: (StrategyLocation, LabelInfo) -> [String] 
           -> StrategyConfiguration -> [ConfigAction]
getActions (loc, info) groups = map snd . filter (select . fst)
 where
   select (ByName s)     = labelName info == s
   select (ByGroup s)    = s `elem` groups
   select (ByLocation l) = loc == l

doAction :: ConfigAction -> LabelInfo -> LabelInfo
doAction action =
   case action of
      Hide   -> setHidden True
      Expose -> setHidden False
      Fold   -> setFolded True
      Unfold -> setFolded False
      Skip   -> setSkipped True
      Insert -> setSkipped False

---------------------------------------------------------------------
-- Configuration combinators

hide, expose :: IsLabeled f => f a -> LabeledStrategy a
hide   = changeInfo (doAction Hide)
expose = changeInfo (doAction Expose)

fold, unfold :: IsLabeled f => f a -> LabeledStrategy a
fold   = changeInfo (doAction Fold)
unfold = changeInfo (doAction Unfold)

skip, insert :: IsLabeled f => f a -> LabeledStrategy a
skip   = changeInfo (doAction Skip)
insert = changeInfo (doAction Insert)

-- helpers
setHidden, setFolded, setSkipped :: Bool -> LabelInfo -> LabelInfo
setHidden  b info = info {hidden  = b}
setFolded  b info = info {folded  = b}
setSkipped b info = info {skipped = b}