{-# LANGUAGE GADTs, Rank2Types #-}
-----------------------------------------------------------------------------
-- Copyright 2015, Ideas project team. This file is distributed under the
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
   , transPure, transMaybe, transList, transEnvMonad
   , transRewrite, transRef
     -- * Lifting transformations
   , transUseEnvironment
   , transLiftView, transLiftViewIn
   , transLiftContext, transLiftContextIn
   , makeTransLiftContext, makeTransLiftContext_
     -- * Using transformations
   , transApply, transApplyWith
   , getRewriteRules, isZeroTrans
   ) where

import Control.Arrow
import Data.Maybe
import Data.Typeable
import Ideas.Common.Classes
import Ideas.Common.Context
import Ideas.Common.Environment
import Ideas.Common.Rewriting
import Ideas.Common.Rule.EnvironmentMonad
import Ideas.Common.Utils
import Ideas.Common.View
import qualified Control.Category as C

-----------------------------------------------------------
--- Trans data type and instances

data Trans a b where
   Zero     :: Trans a b
   List     :: (a -> [b]) -> Trans a b
   Rewrite  :: RewriteRule a -> Trans a a
   EnvMonad :: (a -> EnvMonad b) -> Trans a b
   Ref      :: Typeable a => Ref a -> Trans a a
   UseEnv   :: Trans a b -> Trans (a, Environment) (b, Environment)
   (:>>:)   :: Trans a b -> Trans b c -> Trans a c
   (:**:)   :: Trans a c -> Trans b d -> Trans (a, b) (c, d)
   (:++:)   :: Trans a c -> Trans b d -> Trans (Either a b) (Either c d)
   Apply    :: Trans (Trans a b, a) b
   Append   :: Trans a b -> Trans a b -> Trans a b

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

instance ArrowApply Trans where
   app = Apply

instance Monoid (Trans a b) where
   mempty  = zeroArrow
   mappend = (<+>)

type Transformation a = Trans a a

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

instance MakeTrans EnvMonad where
   makeTrans = transEnvMonad

transPure :: (a -> b) -> Trans a b
transPure f = transList (return . f)

transMaybe :: (a -> Maybe b) -> Trans a b
transMaybe f = transList (maybeToList . f)

transList :: (a -> [b]) -> Trans a b
transList = List

transEnvMonad :: (a -> EnvMonad b) -> Trans a b
transEnvMonad = EnvMonad

transRewrite :: RewriteRule a -> Trans a a
transRewrite = Rewrite

transRef :: Typeable a => Ref a -> Trans a a
transRef = Ref

-----------------------------------------------------------
--- Lifting transformations

transUseEnvironment :: Trans a b -> Trans (a, Environment) (b, Environment)
transUseEnvironment = UseEnv

transLiftView :: View a b -> Transformation b -> Transformation a
transLiftView v = transLiftViewIn (v &&& identity)

transLiftViewIn :: View a (b, c) -> Transformation b -> Transformation a
transLiftViewIn v f = makeTrans (match v) >>> first f >>> arr (build v)

transLiftContext :: Transformation a -> Transformation (Context a)
transLiftContext = transLiftContextIn . transUseEnvironment

transLiftContextIn :: Transformation (a, Environment) -> Transformation (Context a)
transLiftContextIn = transLiftViewIn (contextView >>> (f <-> g))
 where
   f (a, c)        = ((a, environment c), c)
   g ((a, env), c) = (a, setEnvironment env c)

-- | Overloaded variant of @transLiftContext@
makeTransLiftContext :: MakeTrans f => (a -> f a) -> Transformation (Context a)
makeTransLiftContext = transLiftContext . makeTrans

-- | Overloaded variant of @transLiftContext@; ignores result
makeTransLiftContext_ :: MakeTrans f => (a -> f ()) -> Transformation (Context a)
makeTransLiftContext_ f = transLiftContext (identity &&& makeTrans f >>> arr fst)

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
      EnvMonad f -> runEnvMonad (f a) env
      Ref ref    -> case ref ? env of
                       Just b  -> [(b, env)]
                       Nothing -> [(a, insertRef ref a env)]
      UseEnv f   -> do (b, envb) <- transApplyWith (snd a) f (fst a)
                       return ((b, envb), env)
      f :>>: g   -> do (b, env1) <- transApplyWith env  f a
                       (c, env2) <- transApplyWith env1 g b
                       return (c, env2)
      f :**: g   -> do (b, env1) <- transApplyWith env f (fst a)
                       (c, env2) <- transApplyWith env g (snd a)
                       return ((b, c), env2 `mappend` env1)
      f :++: g   -> either (make Left f) (make Right g) a
      Apply      -> uncurry (transApplyWith env) a
      Append f g -> transApplyWith env f a ++ transApplyWith env g a
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
         Ref r      -> [Some r]
         EnvMonad f -> envMonadFunctionRefs f
         _          -> descendTrans allRefs trans

isZeroTrans :: Trans a b -> Bool
isZeroTrans = or . rec
 where
   rec :: Trans a b -> [Bool]
   rec trans =
      case trans of
         Zero       -> [True]
         Append f g -> [isZeroTrans f && isZeroTrans g]
         _          -> descendTrans rec trans

-- General recursion function (existentially quantified)
descendTrans :: Monoid m => (forall x y . Trans x y -> m) -> Trans a b -> m
descendTrans make trans =
   case trans of
      UseEnv f   -> make f
      f :>>: g   -> make f `mappend` make g
      f :**: g   -> make f `mappend` make g
      f :++: g   -> make f `mappend` make g
      Append f g -> make f `mappend` make g
      _          -> mempty
