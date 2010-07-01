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
module Domain.Logic.Views where

import Common.View
import Domain.Logic.Formula

------------------------------------------------------------
-- Smart constructors

infixr 2 .<->.
infixr 3 .->. 
infixr 4 .||. 
infixr 5 .&&.

(.<->.) :: Logic a -> Logic a -> Logic a
T .<->. q = q
F .<->. q = nott q
p .<->. T = p
p .<->. F = nott p
p .<->. q = p :<->: q

(.->.) :: Logic a -> Logic a -> Logic a
T .->. q = q
F .->. _ = T
_ .->. T = T
p .->. F = nott p
p .->. q = p :->: q

(.||.) :: Logic a -> Logic a -> Logic a 
T .||. _ = T
F .||. q = q
_ .||. T = T
p .||. F = p
p .||. q = p :||: q

(.&&.) :: Logic a -> Logic a -> Logic a
T .&&. q = q
F .&&. _ = F
p .&&. T = p
_ .&&. F = F
p .&&. q = p :&&: q

nott :: Logic a -> Logic a
nott (Not p) = p
nott p       = Not p

-------------------------------------------------
-- Views and transformations

simplify :: Logic a -> Logic a
simplify = foldLogic (Var, (.->.), (.<->.), (.&&.), (.||.), nott, T, F)

pushNotWith :: (a -> Logic a) -> Logic a -> Logic a
pushNotWith f = foldLogic (Var, (.->.), (.<->.), (.&&.), (.||.), rec, T, F)
 where
   rec logic = 
      case logic of
         Not p :<->: q -> p     .<->. q
         p :<->: Not q -> p     .<->. q
         p :<->: q     -> rec p .<->. q
         p :->:  q     -> p     .&&.  rec q
         p :||:  q     -> rec p .&&.  rec q
         p :&&:  q     -> rec p .||.  rec q
         Not p         -> p
         T             -> F
         F             -> T
         Var a         -> f a

pushNot :: Logic a -> Logic a
pushNot = pushNotWith (nott . Var)
         
orView :: View (Logic a) [a]
orView = newView "logic.orView" (($ []) . f) (foldr ((.||.). Var) F)
 where
   f (p :||: q) = (>>= f p) .  (f q)
   f (Var a)    = (return . (a:))
   f F          = return
   f _          = const Nothing

andView :: View (Logic a) [a]
andView = newView "logic.andView" (($ []) . f) (foldr ((.&&.). Var) T)
 where
   f (p :&&: q) = (>>= f p) .  (f q)
   f (Var a)    = (return . (a:))
   f T          = return
   f _          = const Nothing