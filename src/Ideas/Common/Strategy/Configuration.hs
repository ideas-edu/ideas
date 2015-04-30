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
   , removeCore, collapseCore, hideCore, configDefs
   , module Data.Monoid
   ) where

import Data.Char
import Data.Monoid
import Ideas.Common.Id
import Ideas.Common.Strategy.Abstract
import Ideas.Common.Rule
import Ideas.Common.Classes
import Ideas.Common.Strategy.Choice
import Ideas.Common.Strategy.Def
import Ideas.Common.Strategy.Sequence
import Ideas.Common.Strategy.Prefix
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
configureS cfg = fromCore . configureCore cfg . toCore

configureCore :: StrategyCfg -> Core a -> Core a
configureCore (Cfg pairs) core = foldr handle core pairs
 where
   handle (ByName l, action) = 
      case action of 
         Remove   -> insertAtLabel l removeDef
         Reinsert -> removeAtLabel l removeDef
         Collapse -> insertAtLabel l collapseDef
         Expand   -> removeAtLabel l collapseDef
         Hide     -> insertAtLabel l hideDef
         Reveal   -> removeAtLabel l hideDef

insertAtLabel :: HasId a => Id -> d -> CyclicTree d a -> CyclicTree d a
insertAtLabel n def = replaceLeaf f . replaceLabel g
 where
   f a | n == getId a = node def [leaf a]
       | otherwise    = leaf a
       
   g l a | n == l    = node def [Tree.label l a]
         | otherwise = Tree.label l a

removeAtLabel :: HasId a => Id -> Def -> CyclicTree Def a -> CyclicTree Def a
removeAtLabel n def = replaceNode $ \d xs -> 
   case map nextId xs of
      [Just l] | n == l -> head xs
      _ -> node d xs

nextId :: HasId a => CyclicTree Def a -> Maybe Id
nextId = fold monoidAlg 
   { fNode  = \d xs -> if isProperty d && length xs == 1 
                       then head xs 
                       else Nothing
   , fLeaf  = \a    -> Just (getId a)
   , fLabel = \l _  -> Just l
   } 

---------------------------------------------------------------------
-- Combinator definitions

removeCore, collapseCore, hideCore :: Core a -> Core a
removeCore   = node1 removeDef
collapseCore = node1 collapseDef
hideCore     = node1 hideDef

configDefs :: [Def]
configDefs = [removeDef, collapseDef, hideDef]

removeDef :: Def
removeDef = propertyDef "removed" (const empty)

collapseDef :: Def
collapseDef = propertyDef "collapsed" (collapse . toProcess)
 where
   collapse a = 
      case firsts a of
         [(Enter l, _)] -> collapseWith l a
         _              -> empty
    
   collapseWith l = 
      single . RuleStep mempty . makeRule l . runProcess

hideDef :: Def
hideDef = propertyDef "hidden" (fmap minor)