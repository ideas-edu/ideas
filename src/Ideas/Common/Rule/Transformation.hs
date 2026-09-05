{-# LANGUAGE GADTs, Rank2Types #-}
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
-- This module defines transformations. Given a term, a transformation returns
-- a list of results (often a singleton list or the empty list).
--
-----------------------------------------------------------------------------

module Ideas.Common.Rule.Transformation
   ( -- * Trans data type
     Transformation, Trans
     -- * Constructor functions
   , MakeTrans(..)
   , transPure, transMaybe, transList, transGuard, transRewrite
     -- * Reading and writing (with references)
   , readRef, readRefDefault, readRefMaybe
   , writeRef, writeRef_, writeRefMaybe
     -- * Lifting transformations
   , transUseEnvironment
   , transLiftView, transLiftViewIn, transLift
   , transLiftContext, transLiftContextIn
     -- * Using transformations
   , transApply, transApplyWith
   , getRewriteRules
   ) where

import Control.Applicative
import Control.Arrow
import Data.Maybe
import Data.Semigroup as Sem
import Ideas.Common.Classes
import Ideas.Common.Context
import Ideas.Common.Environment
import Ideas.Common.Rewriting
import Ideas.Common.View
import Ideas.Utils.Prelude
import qualified Control.Category as C

-----------------------------------------------------------
--- Trans data type and instances

type Transformation a = Trans a a

data Trans a b where
   Zero     :: Trans a b
   List     :: (a -> [b]) -> Trans a b
   Rewrite  :: RewriteRule a -> Trans a a
   UseEnv   :: Trans a b -> Trans (a, Environment) (b, Environment)
   (:>>:)   :: Trans a b -> Trans b c -> Trans a c
   (:**:)   :: Trans a c -> Trans b d -> Trans (a, b) (c, d)
   (:++:)   :: Trans a c -> Trans b d -> Trans (Either a b) (Either c d)
   Append   :: Trans a b -> Trans a b -> Trans a b

   ReadRefM  :: Ref a -> Trans x (Maybe a)
   WriteRefM :: Ref a -> Trans (Maybe a) ()

instance C.Category Trans where
   id  = arr id
   (.) = flip (:>>:)

instance Arrow Trans where
   arr      = transPure
   (***)    = (:**:)
   first  f = f :**: identity
   second f = identity :**: f

instance ArrowZero Trans where
   zeroArrow = Zero

instance ArrowPlus Trans where
   (<+>) = Append

instance ArrowChoice Trans where
   (+++)   = (:++:)
   left f  = f :++: identity
   right f = identity :++: f

instance Sem.Semigroup (Trans a b) where
   (<>) = (<+>)

instance Monoid (Trans a b) where
   mempty  = zeroArrow
   mappend = (<>)

instance Functor (Trans a) where
   fmap f t = t >>^ f

instance Applicative (Trans a) where
   pure    = transPure . const
   s <*> t = (s &&& t) >>^ uncurry ($)

instance Alternative (Trans a) where
   empty = zeroArrow
   (<|>) = (<+>)

-----------------------------------------------------------
--- Constructor functions

-- | A type class for constructing a transformation. If possible, @makeTrans@
-- should be used. Use specialized constructor functions for disambiguation.
class MakeTrans f where
   makeTrans :: (a -> f b) -> Trans a b

instance MakeTrans Maybe where
   makeTrans = transMaybe

instance MakeTrans [] where
   makeTrans = transList

transPure :: (a -> b) -> Trans a b
transPure f = transList (return . f)

transMaybe :: (a -> Maybe b) -> Trans a b
transMaybe f = transList (maybeToList . f)

transList :: (a -> [b]) -> Trans a b
transList = List

transGuard :: (a -> Bool) -> Trans a a
transGuard p = transMaybe $ \x -> if p x then Just x else Nothing

transRewrite :: RewriteRule a -> Trans a a
transRewrite = Rewrite

-----------------------------------------------------------
-- Reading and writing (with references)

readRef :: Ref a -> Trans x a
readRef r = readRefMaybe r >>> transMaybe id

readRefDefault :: a -> Ref a -> Trans x a
readRefDefault a r = readRefMaybe r >>^ fromMaybe a

readRefMaybe  :: Ref a -> Trans x (Maybe a)
readRefMaybe = ReadRefM

writeRef :: Ref a -> Trans a a
writeRef r = (identity &&& writeRef_ r) >>^ fst

writeRef_ :: Ref a -> Trans a ()
writeRef_ r = Just ^>> writeRefMaybe r

writeRefMaybe :: Ref a -> Trans (Maybe a) ()
writeRefMaybe = WriteRefM

-----------------------------------------------------------
-- Lifting transformations

transUseEnvironment :: Trans a b -> Trans (a, Environment) (b, Environment)
transUseEnvironment = UseEnv

transLiftView :: View a b -> Transformation b -> Transformation a
transLiftView v = transLiftViewIn (v &&& identity)

transLiftViewIn :: View a (b, c) -> Transformation b -> Transformation a
transLiftViewIn v f = makeTrans (match v) >>> first f >>^ build v

transLift :: (a -> Maybe (b, b -> a)) -> Transformation b -> Transformation a
transLift f t = makeTrans f >>> first t >>^ (\(x, g) -> g x)

transLiftContext :: Transformation a -> Transformation (Context a)
transLiftContext = transLiftContextIn . transUseEnvironment

transLiftContextIn :: Transformation (a, Environment) -> Transformation (Context a)
transLiftContextIn = transLiftViewIn (contextView >>> (f <-> g))
 where
   f (a, c)        = ((a, environment c), c)
   g ((a, env), c) = (a, setEnvironment env c)

-----------------------------------------------------------
--- Using transformations

transApply :: Trans a b -> a -> [(b, Environment)]
transApply = transApplyWith mempty

transApplyWith :: Environment -> Trans a b -> a -> [(b, Environment)]
transApplyWith env trans a =
   case trans of
      Zero       -> []
      List f     -> [ (b, env) | b <- f a ]
      Rewrite r  -> [ (b, env) | b <- applyAll r a ]
      UseEnv f   -> do (b, envb) <- transApplyWith (snd a) f (fst a)
                       return ((b, envb), env)
      f :>>: g   -> do (b, env1) <- transApplyWith env  f a
                       (c, env2) <- transApplyWith env1 g b
                       return (c, env2)
      f :**: g   -> do (b, env1) <- transApplyWith env f (fst a)
                       (c, env2) <- transApplyWith env1 g (snd a)
                       return ((b, c), env2)
      f :++: g   -> either (make Left f) (make Right g) a
      Append f g -> transApplyWith env f a ++ transApplyWith env g a
      ReadRefM r  -> [(r ? env, env)]
      WriteRefM r -> [((), maybe (deleteRef r) (insertRef r) a env)]
 where
   make :: (b -> c) -> Trans a b -> a -> [(c, Environment)]
   make f g = map (mapFirst f) . transApplyWith env g

getRewriteRules :: Trans a b -> [Some RewriteRule]
getRewriteRules trans =
   case trans of
      Rewrite r -> [Some r]
      _         -> descendTrans getRewriteRules trans

instance HasRefs (Trans a b) where
   allRefs trans =
      case trans of
         ReadRefM r  -> [Some r]
         WriteRefM r -> [Some r]
         _           -> descendTrans allRefs trans

-- General recursion function (existentially quantified)
descendTrans :: Monoid m => (forall x y . Trans x y -> m) -> Trans a b -> m
descendTrans make trans =
   case trans of
      UseEnv f     -> make f
      f :>>: g     -> make f `mappend` make g
      f :**: g     -> make f `mappend` make g
      f :++: g     -> make f `mappend` make g
      Append f g   -> make f `mappend` make g
      _            -> mempty