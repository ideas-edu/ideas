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
-----------------------------------------------------------------------------
module Common.Strategy.Configuration 
   ( -- Types and constructors
     StrategyConfiguration, ConfigItem
   , ConfigLocation(..), ConfigAction(..), configActions
     --  Configure
  ,  configure
     -- Combinators
   , remove, reinsert, collapse, expand, hide, reveal
   ) where

import Common.Strategy.Abstract
import Common.Strategy.Core
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
 
data ConfigAction = Remove | Reinsert | Collapse | Expand | Hide | Reveal
   deriving (Show, Enum)

configActions :: [ConfigAction]
configActions = [Remove .. ]

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
      Remove   -> setRemoved True
      Reinsert -> setRemoved False
      Collapse -> setCollapsed True
      Expand   -> setCollapsed False
      Hide     -> setHidden True
      Reveal   -> setHidden False

---------------------------------------------------------------------
-- Configuration combinators

remove, reinsert :: IsLabeled f => f a -> LabeledStrategy a
remove   = changeInfo (doAction Remove)
reinsert = changeInfo (doAction Reinsert)

collapse, expand :: IsLabeled f => f a -> LabeledStrategy a
collapse = changeInfo (doAction Collapse)
expand   = changeInfo (doAction Expand)

hide, reveal :: IsLabeled f => f a -> LabeledStrategy a
hide   = changeInfo (doAction Hide)
reveal = changeInfo (doAction Reveal)

-- helpers
setRemoved, setCollapsed, setHidden :: Bool -> LabelInfo -> LabelInfo
setRemoved   b info = info {removed   = b}
setCollapsed b info = info {collapsed = b}
setHidden    b info = info {hidden    = b}