{-# LANGUAGE TypeFamilies #-}
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
-- Strategies can be configured at their labeled positions. Possible actions
-- are remove/reinsert, collapse/expand, and hide/reveal.
--
-----------------------------------------------------------------------------

module Ideas.Common.Strategy.Configuration
   ( StrategyCfg, byName, ConfigAction(..)
   , configure, configureS
   , remove, collapse, hide, multi
   , isConfigId
   ) where

import Data.Char
import Data.Semigroup as Sem
import Ideas.Common.Classes
import Ideas.Common.Id
import Ideas.Common.Rule
import Ideas.Common.Strategy.Abstract
import Ideas.Common.Strategy.Choice
import Ideas.Common.Strategy.CyclicTree hiding (label)
import Ideas.Common.Strategy.Derived (repeat1)
import Ideas.Common.Strategy.Process hiding (fold)
import Ideas.Common.Strategy.Sequence
import Ideas.Common.Strategy.StrategyTree
import Ideas.Common.Strategy.Symbol
import qualified Ideas.Common.Strategy.CyclicTree as Tree

---------------------------------------------------------------------
-- Types and constructors

newtype StrategyCfg = Cfg [(ConfigLocation, ConfigAction)]

instance Show StrategyCfg where
   show (Cfg xs) = show xs

instance Sem.Semigroup StrategyCfg where
   (Cfg xs) <> (Cfg ys) = Cfg (xs ++ ys)

instance Monoid StrategyCfg where
   mempty  = Cfg []
   mconcat xs = Cfg [ y | Cfg ys <- xs, y <- ys ]
   mappend = (<>)

newtype ConfigLocation = ByName Id

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
         Remove   -> insertAtLabel l removeDecl
         Reinsert -> removeAtLabel l removeDecl
         Collapse -> insertAtLabel l collapseDecl
         Expand   -> removeAtLabel l collapseDecl
         Hide     -> insertAtLabel l hideDecl
         Reveal   -> removeAtLabel l hideDecl

insertAtLabel :: Id -> Decl Unary -> StrategyTree a -> StrategyTree a
insertAtLabel n comb = replaceLeaf f . replaceLabel g
 where
   f a | n == getId a = fromUnary (applyDecl comb) (leaf a)
       | otherwise    = leaf a

   g l a | n == l    = fromUnary (applyDecl comb) (Tree.label l a)
         | otherwise = Tree.label l a

removeAtLabel :: Id -> Decl Unary -> StrategyTree a -> StrategyTree a
removeAtLabel n _decl = replaceNode $ \d xs -> -- fix me: use decl
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
isConfigId = (`elem` map getId configIds) . getId

---------------------------------------------------------------------
-- Combinator definitions

remove, collapse, hide :: IsStrategy f => f a -> Strategy a
remove   = decl1 removeDecl
collapse = decl1 collapseDecl
hide     = decl1 hideDecl

-- | Apply a strategy at least once, but collapse into a single step
multi :: (IsId l, IsStrategy f) => l -> f a -> Strategy a
multi l = collapse . label l . decl1 repeatDecl . toStrategy

repeatDecl :: Decl Unary -- fix me: overlap with combinators
repeatDecl = "repeat1" .=. Unary repeat1

configIds :: [Id]
configIds = map getId [removeDecl, collapseDecl, hideDecl]

removeDecl :: Decl Unary
removeDecl = "removed" .=. Unary (const empty)

collapseDecl :: Decl Unary
collapseDecl = "collapsed" .=. Unary f
 where
   f a = case firsts a of
            [(LeafRule r, _)] -> maybe empty (`collapseWith` a) (isEnterRule r)
            _ -> empty
   collapseWith l =
      single . LeafRule . makeRule l . runProcess

hideDecl :: Decl Unary
hideDecl = "hidden" .=. Unary (fmap minor)