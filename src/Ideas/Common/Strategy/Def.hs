{-# LANGUAGE RankNTypes #-}
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
-- The core strategy combinators. This module defines the interal data
-- structure of a strategy, and some utility functions that operate
-- directly on it.
--
-----------------------------------------------------------------------------
--  $Id: Core.hs 7590 2015-04-21 07:26:58Z bastiaan $

module Ideas.Common.Strategy.Def
   ( Def, makeDef, makeDef1, makeDef2
   , associativeDef, propertyDef
   , isAssociative, isProperty
   , useDef
   ) where

import Ideas.Common.Id
import Ideas.Common.Rule
import Ideas.Common.Strategy.Choice
import Ideas.Common.Strategy.Process
import Prelude hiding (sequence)

data Def = Def
   { defId         :: Id 
   , isAssociative :: Bool 
   , isProperty    :: Bool
   , useDef        :: forall a . [Builder (Rule a)] -> Builder (Rule a)
   }

instance Show Def where
   show = showId

makeDef :: IsId n => n -> (forall a . [Builder (Rule a)] -> Builder (Rule a)) -> Def
makeDef n = Def (newId n) False False

makeDef1 :: IsId n => n -> (forall a . Builder (Rule a) -> Builder (Rule a)) -> Def
makeDef1 n f = makeDef n $ \xs -> 
   case xs of
      [a] -> f a
      _   -> empty

makeDef2 :: IsId n => n -> (forall a . Builder (Rule a) -> Builder (Rule a) -> Builder (Rule a)) -> Def
makeDef2 n f = makeDef n $ \xs -> 
   case xs of
      [a, b] -> f a b
      _      -> empty

associativeDef :: IsId n => n -> (forall a . [Builder (Rule a)] -> Builder (Rule a)) -> Def
associativeDef n f = (makeDef n f) { isAssociative = True }

propertyDef :: IsId n => n -> (forall a . Builder (Rule a) -> Builder (Rule a)) -> Def
propertyDef n f = (makeDef1 n f) { isProperty = True }

instance Eq Def where
   x == y = compareId x y == EQ

instance HasId Def where
   getId = defId
   changeId f d = d { defId = f (defId d) }