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
   , Location, location
     -- * Lifting
   , liftToContext, contextView
   , use, useC, termNavigator, applyTop
   , currentTerm, replaceInContext, currentInContext, changeInContext
   ) where

import Common.Environment
import Common.Id
import Common.Focus
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
   , getNavigator   :: MyNavigator (Maybe a) -- ^ Value with focus
   }

fromContext :: Monad m => Context a -> m a
fromContext c = maybe (fail "fromContext") return $ leave (getNavigator c)

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

instance Navigator (Context a) where
   up       (C env a) = liftM (C env) (up a)
   downs    (C env a) = map (C env) (downs a)
   location (C _   a) = location a

instance HasEnvironment (Context a) where
   environment = getEnvironment
   setEnvironment e c = c {getEnvironment = e}

-- | Construct a context
newContext :: Environment -> MyNavigator (Maybe a) -> Context a
newContext = C

----------------------------------------------------------
-- Lifting rules

contextView :: View (Context a) (a, Context a)
contextView = "views.contextView" @> makeView f g
 where
   f ctx = currentInContext ctx >>= \a -> Just (a, ctx)
   g     = uncurry replaceInContext

-- | Lift a rule to operate on a term in a context
liftToContext :: LiftView f => f a -> f (Context a)
liftToContext = liftViewIn contextView
   
-- | Apply a function at top-level. Afterwards, try to return the focus
-- to the old position
applyTop :: (a -> a) -> Context a -> Context a
applyTop f c =
   navigateTowards (location c) (changeInContext f (top c))

termNavigator :: IsTerm a => a -> MyNavigator (Maybe a)
termNavigator = viewNavigator

use :: (LiftView f, IsTerm a, IsTerm b) => f a -> f (Context b)
use = useC . liftToContext

useC :: (LiftView f, IsTerm a, IsTerm b) => f (Context a) -> f (Context b)
useC = liftView (makeView (Just . f) f)
 where
   f :: IsTerm b => Context a -> Context b
   f (C env a) = C env (castT a)
   
currentTerm :: Context a -> Maybe Term
currentTerm = currentT . getNavigator

currentInContext :: Context a -> Maybe a
currentInContext (C _   a) = current a

changeInContext :: (a -> a) -> Context a -> Context a
changeInContext f (C env a) = C env (change (fmap f) a)

replaceInContext :: a -> Context a -> Context a
replaceInContext = changeInContext . const