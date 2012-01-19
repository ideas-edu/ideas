-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- A context for a term that maintains an environment of
-- key-value pairs. A context is both showable and parsable.
--
-----------------------------------------------------------------------------
module Common.Context
   ( -- * Abstract data type
     Context, newContext
   , fromContext, fromContextWith, fromContextWith2
     -- * Lifting
   , liftToContext
   , use, useC, termNavigator, applyTop
   ) where

import Common.Binding
import Common.Id
import Common.Navigator
import Common.Rewriting
import Common.View
import Control.Monad
import Data.Maybe

----------------------------------------------------------
-- Abstract data type

-- | Abstract data type for a context: a context stores an envrionent
-- (key-value pairs) and a value
data Context a = C
   { getEnvironment :: Environment -- ^ Returns the environment
   , getNavigator   :: Navigator a -- ^ Retrieve a value from its context
   }

fromContext :: Monad m => Context a -> m a
fromContext = leave . getNavigator

fromContextWith :: Monad m => (a -> b) -> Context a -> m b
fromContextWith f = liftM f . fromContext

fromContextWith2 :: Monad m => (a -> b -> c) -> Context a -> Context b -> m c
fromContextWith2 f a b = liftM2 f (fromContext a) (fromContext b)

instance Eq a => Eq (Context a) where
   x == y = fromMaybe False $ liftM2 (==) (fromContext x) (fromContext y)

instance Show a => Show (Context a) where
   show (C env a) =
      let rest | noBindings env = ""
               | otherwise      = "  {" ++ show env ++ "}"
      in show a ++ rest

instance IsNavigator Context where
   up        (C env a) = liftM (C env) (up a)
   allDowns  (C env a) = map (C env) (allDowns a)
   current   (C _   a) = current a
   location  (C _   a) = location a
   changeM f (C env a) = liftM (C env) (changeM f a)

instance TypedNavigator Context where
   changeT f (C env a) = liftM (C env) (changeT f a)
   currentT  (C _   a) = currentT a
   leaveT    (C _   a) = leaveT a
   castT v   (C env a) = liftM (C env) (castT v a)

instance HasEnvironment (Context a) where
   environment     = getEnvironment
   insertRef r a c = c {getEnvironment = insertRef r a (environment c)}
   deleteRef r c   = c {getEnvironment = deleteRef r (environment c)}

-- | Construct a context
newContext :: Environment -> Navigator a -> Context a
newContext = C

----------------------------------------------------------
-- Lifting rules

-- | Lift a rule to operate on a term in a context
liftToContext :: LiftView f => f a -> f (Context a)
liftToContext = liftViewIn cv
 where
   cv    = "views.contextView" @> makeView f g
   f ctx = current ctx >>= \a -> Just (a, ctx)
   g     = uncurry replace
   
-- | Apply a function at top-level. Afterwards, try to return the focus
-- to the old position
applyTop :: (a -> a) -> Context a -> Context a
applyTop f c =
   case top c of
      Just ok -> navigateTowards (location c) (change f ok)
      Nothing -> c

termNavigator :: IsTerm a => a -> Navigator a
termNavigator a = fromMaybe (noNavigator a) (make a)
 where
   make = castT termView . viewNavigatorWith spineHoles . toTerm

   spineHoles :: Term -> [(Term, Term -> Term)]
   spineHoles term
      | null xs   = []
      | otherwise = (x, flip makeTerm xs) : zipWith f [0..] xs
    where
      (x, xs)    = getSpine term
      f i y      = (y, makeTerm x . changeAt i)
      changeAt i b =
         case splitAt i xs of
            (ys, _:zs) -> ys ++ b:zs
            _          -> xs

use :: (LiftView f, IsTerm a, IsTerm b) => f a -> f (Context b)
use = useC . liftToContext

useC :: (LiftView f, IsTerm a, IsTerm b) => f (Context a) -> f (Context b)
useC = liftView (makeView (castT termView) (fromJust . castT termView))