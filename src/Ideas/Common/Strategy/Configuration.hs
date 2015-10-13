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
-----------------------------------------------------------------------------
--  $Id$

module Ideas.Common.Strategy.Configuration
   ( StrategyCfg, byName, ConfigAction(..)
   , configure, configureS
   , remove, collapse, hide
   , isConfigId
   , module Data.Monoid
   ) where

import Data.Char
import Data.Monoid
import Ideas.Common.Id
import Ideas.Common.Strategy.Abstract
import Ideas.Common.Rule
import Ideas.Common.Classes
import Ideas.Common.Strategy.Choice
import Ideas.Common.Strategy.Sequence
import Ideas.Common.Strategy.Process hiding (fold)
import Ideas.Common.CyclicTree hiding (label)
import Ideas.Common.Strategy.Step
import qualified Ideas.Common.CyclicTree as Tree

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
configureS = onStrategyTree . configureStrategyTree

configureStrategyTree :: StrategyCfg -> StrategyTree a -> StrategyTree a
configureStrategyTree (Cfg pairs) tree = foldr handle tree pairs
 where
   handle (ByName l, action) = 
      case action of 
         Remove   -> insertAtLabel l removeDef
         Reinsert -> removeAtLabel l removeDef
         Collapse -> insertAtLabel l collapseDef
         Expand   -> removeAtLabel l collapseDef
         Hide     -> insertAtLabel l hideDef
         Reveal   -> removeAtLabel l hideDef

insertAtLabel :: Id -> Combinator f -> StrategyTree a -> StrategyTree a
insertAtLabel n comb = replaceLeaf f . replaceLabel g
 where
   f a | n == getId a = useDef comb [leaf a]
       | otherwise    = leaf a
       
   g l a | n == l    = useDef comb [Tree.label l a]
         | otherwise = Tree.label l a

removeAtLabel :: Id -> Combinator f -> StrategyTree a -> StrategyTree a
removeAtLabel n _def = replaceNode $ \d xs -> -- fix me: use def
   case map nextId xs of
      [Just l] | n == l -> head xs
      _ -> node d xs

nextId :: StrategyTree a -> Maybe Id
nextId = fold monoidAlg 
   { fNode  = \d xs -> if isConfigId d && length xs == 1 
                       then head xs 
                       else Nothing
   , fLeaf  = Just . getId
   , fLabel = \l _  -> Just l
   } 

isConfigId :: HasId a => a -> Bool
isConfigId = (`elem` map getId configDefs) . getId

---------------------------------------------------------------------
-- Combinator definitions

remove :: IsStrategy f => f a -> Strategy a
remove = liftS (useCombinator removeDef)

collapse :: IsStrategy f => f a -> Strategy a
collapse = liftS (useCombinator collapseDef)

hide :: IsStrategy f => f a -> Strategy a
hide = liftS (useCombinator hideDef)

configDefs :: [Id]
configDefs = [getId removeDef, getId collapseDef, getId hideDef]

removeDef :: Combinator (Strategy a -> Strategy a)
removeDef = combinator1 "removed" (const empty)

collapseDef :: Combinator (Strategy a -> Strategy a)
collapseDef = combinator1 "collapsed" $ \a -> 
   case firsts a of
      [(r, _)] -> maybe empty (`collapseWith` a) (isEnterRule r)
      _        -> empty
 where    
   collapseWith l = 
      single . makeRule l . runProcess

hideDef :: Combinator (Strategy a -> Strategy a)
hideDef = combinator1 "hidden" (fmap minor)