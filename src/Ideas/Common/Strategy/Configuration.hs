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
-----------------------------------------------------------------------------
module Ideas.Common.Strategy.Configuration
   ( -- Types and constructors
     StrategyConfiguration, makeStrategyConfiguration
   , ConfigItem, ConfigLocation, byName, byGroup
   , ConfigAction(..), configActions
     --  Configure
  ,  configure, configureNow
     -- Combinators
   , remove, reinsert, collapse, expand, hide, reveal
   ) where

import Data.Maybe
import Ideas.Common.Classes
import Ideas.Common.Id
import Ideas.Common.Strategy.Abstract
import Ideas.Common.Strategy.Core

---------------------------------------------------------------------
-- Types and constructors

newtype StrategyConfiguration = SC { configItems :: [ConfigItem] }
   deriving Show

makeStrategyConfiguration :: [ConfigItem] -> StrategyConfiguration
makeStrategyConfiguration = SC

type ConfigItem = (ConfigLocation, ConfigAction)

data ConfigLocation
   = ByName  Id
   | ByGroup Id
 deriving Show

data ConfigAction = Remove | Reinsert | Collapse | Expand | Hide | Reveal
   deriving (Show, Enum)

configActions :: [ConfigAction]
configActions = [Remove .. ]

byName :: HasId a => a -> ConfigLocation
byName = ByName . getId

byGroup :: HasId a => a -> ConfigLocation
byGroup = ByGroup . getId

---------------------------------------------------------------------
-- Configure

configureNow :: LabeledStrategy a -> LabeledStrategy a
configureNow =
   let lsToCore = toCore . toStrategy
       coreToLS = fromMaybe err . toLabeledStrategy . fromCore
       err      = error "configureNow: label disappeared"
   in coreToLS . processLabelInfo id . lsToCore

configure :: StrategyConfiguration -> LabeledStrategy a -> LabeledStrategy a
configure cfg ls =
   label (getId ls) (fromCore (configureCore cfg (toCore (unlabel ls))))

configureCore :: StrategyConfiguration -> Core LabelInfo a -> Core LabelInfo a
configureCore cfg = mapFirst (change [])
 where
   change groups info =
      let actions = getActions info groups cfg
      in foldr doAction info actions

getActions :: LabelInfo -> [String]
           -> StrategyConfiguration -> [ConfigAction]
getActions info groups = map snd . filter (select . fst) . configItems
 where
   select (ByName a)  = getId info == a
   select (ByGroup s) = showId s `elem` groups

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