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
--  $Id$

module Ideas.Common.Strategy.Configuration
   ( StrategyCfg, byName, ConfigAction(..)
   , configure, configureS
   , module Data.Monoid
   ) where

import Data.Char
import Ideas.Common.Id
import Ideas.Common.Strategy.Abstract
import Ideas.Common.Strategy.Core hiding (Remove, Collapse, Hide)
import Ideas.Common.Utils.Uniplate
import Data.Monoid
import qualified Ideas.Common.Strategy.Core as Core

---------------------------------------------------------------------
-- Types and constructors

newtype StrategyCfg = Cfg [(ConfigLocation, ConfigAction)]

instance Show StrategyCfg where
   show (Cfg xs) = show xs

instance Monoid StrategyCfg where
   mempty  = Cfg []
   mconcat xs = Cfg [ y | Cfg ys <- xs, y <- ys ]
   mappend (Cfg xs) (Cfg ys) = Cfg (xs ++ ys)   

data ConfigLocation = ByName Id

instance Show ConfigLocation where
   show (ByName a) = show a

data ConfigAction = Remove | Reinsert | Collapse | Expand | Hide | Reveal
   deriving (Show, Eq)

instance Read ConfigAction where
   readsPrec _ s = 
      let f = map toLower 
      in [ (x, "") | x <- concat actionGroups, f s == f (show x) ]

actionGroups :: [[ConfigAction]]
actionGroups = [[Remove, Reinsert], [Collapse, Expand], [Hide, Reveal]]

byName :: HasId a => ConfigAction -> a -> StrategyCfg
byName action a = Cfg [(ByName (getId a), action)]

---------------------------------------------------------------------
-- Configure

configure :: StrategyCfg -> LabeledStrategy a -> LabeledStrategy a
configure cfg ls = label (getId ls) (configureS cfg (unlabel ls))

configureS :: StrategyCfg -> Strategy a -> Strategy a
configureS cfg = fromCore . configureCore cfg . toCore

configureCore :: StrategyCfg -> Core a -> Core a
configureCore (Cfg pairs) = rec
 where
   rec core = 
      case core of
         Core.Remove s   | has Reinsert -> rec s
         Core.Collapse s | has Expand   -> rec s
         Core.Hide s     | has Reveal   -> rec s
         Label l s -> props (Label l (rec s))
         _ -> descend rec core
    where
      myLabel  = getLabel core
      actions  = cancel [ a | (loc, a) <- pairs, maybe False (here loc) myLabel ]
      has      = (`elem` actions) 
      make x g = if has x then g else id
      
      props    = make Remove   Core.Remove
               . make Hide     Core.Hide
               . make Collapse Core.Collapse 

here :: ConfigLocation -> Id -> Bool
here (ByName a) info = getId info == a

getLabel :: Core a -> Maybe Id
getLabel (Label l _)       = Just l
getLabel (Core.Remove s)   = getLabel s
getLabel (Core.Collapse s) = getLabel s
getLabel (Core.Hide s)     = getLabel s
getLabel _                 = Nothing

cancel :: [ConfigAction] -> [ConfigAction]
cancel [] = []
cancel (x:xs) = x : cancel (rec actionGroups)
 where
   rec (g:gs)
      | x `elem` g = filter (`notElem` g) xs
      | otherwise  = rec gs
   rec [] = xs
