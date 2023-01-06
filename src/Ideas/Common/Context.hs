{-# LANGUAGE GADTs, RankNTypes #-}
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
--
-- The 'Context' datatype places a value in a context consisting of an
-- environment with bindings and a point of focus. The datatype is an instance
-- of the 'HasEnvironment' type class (for accessing the environment) and
-- the 'Navigator' type class (for traversing the term).
--
-----------------------------------------------------------------------------

module Ideas.Common.Context
   ( -- * Abstract data type
     Context, newContext
   , fromContext, fromContextWith, fromContextWith2
     -- * Context navigator
   , ContextNavigator, noNavigator, navigator, termNavigator
     -- * Lifting
   , liftToContext, contextView
   , use, useC, applyTop
   , currentTerm, changeTerm, replaceInContext, currentInContext, changeInContext
   , setLocation
   ) where

import Data.Maybe
import Ideas.Common.Environment
import Ideas.Common.Id
import Ideas.Common.Rewriting
import Ideas.Common.Traversal.Navigator
import Ideas.Common.Traversal.Utils
import Ideas.Common.View hiding (left, right)
import Ideas.Utils.Uniplate

----------------------------------------------------------
-- Abstract data type

-- | Abstract data type for a context: a context stores an environment.
data Context a = C
   { getEnvironment :: Environment
   , getNavigator   :: ContextNavigator a
   }

fromContext :: Context a -> Maybe a
fromContext = currentNavigator . getNavigator . top

fromContextWith :: (a -> b) -> Context a -> Maybe b
fromContextWith f = fmap f . fromContext

fromContextWith2 :: (a -> b -> c) -> Context a -> Context b -> Maybe c
fromContextWith2 f a b = f <$> fromContext a <*> fromContext b

instance Eq a => Eq (Context a) where
   x == y = fromMaybe False $ (==) <$> fromContext x <*> fromContext y

instance Show a => Show (Context a) where
   show c@(C env a) =
      let rest | noBindings env = ""
               | otherwise      = "  {" ++ show env ++ "}"
      in maybe (maybe "??" show (currentT a)) show (currentNavigator a) ++
         " @ " ++ show (location c) ++ rest

instance Navigator (Context a) where
   up       = liftCN up
   down     = liftCN down
   downLast = liftCN downLast
   left     = liftCN left
   right    = liftCN right
   location = navLocation . getNavigator

instance HasEnvironment (Context a) where
   environment = getEnvironment
   setEnvironment e c = c {getEnvironment = e}

-- | Construct a context
newContext :: ContextNavigator a -> Context a
newContext = C mempty

----------------------------------------------------------
-- Context navigator

noNavigator :: a -> ContextNavigator a
noNavigator = NoNav

navigator :: Uniplate a => a -> ContextNavigator a
navigator = Simple . focus

termNavigator :: IsTerm a => a -> ContextNavigator a
termNavigator = TermNav . focus . toTerm

data ContextNavigator a where
   TermNav :: IsTerm a   => UniplateNavigator Term -> ContextNavigator a
   Simple  :: Uniplate a => UniplateNavigator a -> ContextNavigator a
   NoNav   :: a -> ContextNavigator a

liftCN :: (forall b . Navigator b => b -> Maybe b) -> Context a -> Maybe (Context a)
liftCN f (C env (TermNav a)) = C env . TermNav <$> f a
liftCN f (C env (Simple a))  = C env . Simple  <$> f a
liftCN _ (C _   (NoNav _))   = fail "noNavigator"

navLocation :: ContextNavigator a -> Location
navLocation (TermNav a) = location a
navLocation (Simple a)  = location a
navLocation (NoNav _)   = mempty

currentNavigator :: ContextNavigator a -> Maybe a
currentNavigator (TermNav a) = match termView (current a)
currentNavigator (Simple a)  = Just (current a)
currentNavigator (NoNav a)   = Just a

changeNavigator :: (a -> a) -> ContextNavigator a -> ContextNavigator a
changeNavigator f (TermNav a) = TermNav (change (simplifyWith f termView) a)
changeNavigator f (Simple a)  = Simple (change f a)
changeNavigator f (NoNav a)   = NoNav (f a)

currentT :: ContextNavigator a -> Maybe Term
currentT (TermNav a) = Just (current a)
currentT _           = Nothing

changeT :: (Term -> Maybe Term) -> ContextNavigator a -> Maybe (ContextNavigator a)
changeT f (TermNav a) = TermNav <$> changeM f a
changeT _ _           = Nothing

castT :: IsTerm b => ContextNavigator a -> Maybe (ContextNavigator b)
castT (TermNav a) = Just (TermNav a)
castT _           = Nothing

----------------------------------------------------------
-- Lifting rules

contextView :: View (Context a) (a, Context a)
contextView = "views.contextView" @> makeView f g
 where
   f ctx = currentInContext ctx >>= \a -> Just (a, ctx)
   g     = uncurry replaceInContext

-- | Lift a rule to operate on a term in a context
liftToContext :: Lift f => f a -> f (Context a)
liftToContext = liftViewIn contextView

-- | Apply a function at top-level. Afterwards, try to return the focus
-- to the old position
applyTop :: (a -> a) -> Context a -> Context a
applyTop f c =
   navigateTowards (location c) (changeInContext f (top c))

use :: (Lift f, IsTerm a, IsTerm b) => f a -> f (Context b)
use = useC . liftToContext

useC :: (Lift f, IsTerm a, IsTerm b) => f (Context a) -> f (Context b)
useC = liftViewIn (makeView f g)
 where
   f old@(C env a) = castT a >>= \b -> return (C env b, old)
   g (C env a, old) = maybe old (C env) (castT a)

currentTerm :: Context a -> Maybe Term
currentTerm = currentT . getNavigator

changeTerm :: (Term -> Maybe Term) -> Context a -> Maybe (Context a)
changeTerm f c = do
   new <- changeT f (getNavigator c)
   return c {getNavigator = new}

currentInContext :: Context a -> Maybe a
currentInContext (C _   a) = currentNavigator a

changeInContext :: (a -> a) -> Context a -> Context a
changeInContext f (C env a) = C env (changeNavigator f a)

replaceInContext :: a -> Context a -> Context a
replaceInContext = changeInContext . const

setLocation :: Location -> Context a -> Context a
setLocation loc c = fromMaybe c (navigateTo loc c)