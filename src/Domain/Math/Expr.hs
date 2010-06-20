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
module Domain.Math.Expr 
   ( module Domain.Math.Expr.Data
   , module Domain.Math.Expr.Parser
   , module Domain.Math.Expr.Symbolic
   , module Domain.Math.Expr.Symbols
   , module Domain.Math.Expr.Views
     -- Navigator related functions
   , use, useC, cleanTop, exprNavigator, multi
   ) where

import Domain.Math.Expr.Data
import Domain.Math.Expr.Parser
import Domain.Math.Expr.Symbolic
import Domain.Math.Expr.Symbols
import Domain.Math.Expr.Views

import Common.Context
import Common.Navigator
import Common.Rewriting
import Common.Rewriting.Term
import Common.Strategy
import Common.Transformation
import Common.View
import Data.Maybe

-- New, navigator related functions. 
-- To do: move to a better place
use :: (IsTerm a, IsTerm b) => Rule a -> Rule (Context b)
use = useC . liftToContext

{-
useC :: (IsTerm a, IsTerm b) => Rule (Context a) -> Rule (Context b)
useC = liftRule (makeView (castT exprView) (fromJust . castT exprView))
                                 
exprNavigator :: IsTerm a => a -> Navigator a
exprNavigator a = 
   let f = castT exprView . viewNavigator . toExpr
   in fromMaybe (noNavigator a) (f a)
-}
cleanTop :: (a -> a) -> Context a -> Context a
cleanTop f c = 
   case top c of 
      Just ok -> navigateTowards (location c) (change f ok)
      Nothing -> c

multi :: IsStrategy f => String -> f a -> LabeledStrategy a
multi s = collapse . label s . repeat1

termView :: IsTerm a => View Term a
termView = makeView fromTerm toTerm

useC :: (IsTerm a, IsTerm b) => Rule (Context a) -> Rule (Context b)
useC = liftRule (makeView (castT termView) (fromJust . castT termView))

exprNavigator :: IsTerm a => a -> Navigator a
exprNavigator a = 
   let f = castT termView . viewNavigator . toTerm
   in fromMaybe (noNavigator a) (f a)