{-# LANGUAGE GADTs #-}
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
-- This module defines transformations. Given a term, a transformation returns 
-- a list of results (often a singleton list or the empty list). A 
-- transformation can be parameterized with one or more Bindables. 
-- Transformations rules can be lifted to work on more complex domains with
-- the LiftView type class.
--
-----------------------------------------------------------------------------
module Common.Rule.Transformation
   ( -- * Transformations
     Transformation, transMaybe
   , makeTrans, makeTransG, makeTransEnv, makeRewriteTrans, makeTransEnv_
   , applyTransformation
     -- * Bindables
   , ParamTrans, supplyLocals
   , supplyParameters, supplyContextParameters
   , parameter1, parameter2, parameter3
     -- * Extract information
   , transRewriteRules, transRefs
   ) where

import Common.Environment
import Common.Classes
import Common.Context
import Common.Id
import Common.Navigator
import Common.Rewriting
import Common.Utils
import Common.Rule.EnvironmentMonad
import Common.View
import Control.Monad
import Control.Arrow
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.Typeable
import qualified Control.Category as C

data Trans a b where
   List     :: (a -> [b]) -> Trans a b
   Rewrite  :: RewriteRule a -> (a -> [a]) -> Trans a a
   TransEnv :: (a -> EnvMonad b) -> Trans a b
   Bind     :: Typeable a => Ref a -> Trans a a
   BoxedEnv :: Trans a b -> Trans (a, Environment) (b, Environment)
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
   first  f = f :**: C.id
   second f = C.id :**: f
   
instance ArrowChoice Trans where
   (+++)   = (:++:)
   left f  = f :++: C.id
   right f = C.id :++: f

instance ArrowApply Trans where
   app = Apply

instance Monoid (Trans a b) where
   mempty  = transList (const [])
   mappend = Append

transPure :: (a -> b) -> Trans a b
transPure f = transList (return . f)

transMaybe :: (a -> Maybe b) -> Trans a b
transMaybe f = transList (maybeToList . f)

transList :: (a -> [b]) -> Trans a b
transList = List

transRewrite :: RewriteRule a -> Trans a a
transRewrite r = Rewrite r (applyAll r)

transEnv :: (a -> EnvMonad b) -> Trans a b
transEnv = TransEnv

bindValue :: (IsId n, Reference a) => n -> Trans a a
bindValue = Bind . makeRef

-----------------------------------------------------------
--- Transformations

type Transformation a = Trans a a

applyTransformation :: Trans a b -> a -> [(b, Environment)]
applyTransformation = applyTransformationWith mempty

applyTransformationWith :: Environment -> Trans a b -> a -> [(b, Environment)]
applyTransformationWith = rec 
 where 
   rec :: Environment -> Trans a b -> a -> [(b, Environment)]
   rec env trans a = 
      case trans of
         List f      -> [ (b, env) | b <- f a ]
         Rewrite _ f -> [ (b, env) | b <- f a ]
         TransEnv f  -> runEnvMonad (f a) env
         Bind ref    -> [(a, insertRef ref a env)]
         BoxedEnv f  -> do (b, envb) <- rec (snd a) f (fst a) 
                           return ((b, envb), env)
         f :>>: g    -> do (b, env1) <- rec env  f a
                           (c, env2) <- rec env1 g b
                           return (c, env2)
         f :**: g    -> do (b, env1) <- rec env f (fst a)
                           (c, env2) <- rec env g (snd a)
                           return ((b, c), env2 `mappend` env1)
         f :++: g    -> either (make env Left f) (make env Right g) a
         Apply       -> uncurry (rec env) a
         Append f g  -> rec env f a ++ rec env g a

   make :: Environment -> (b -> c) -> Trans a b -> a -> [(c, Environment)]
   make env f g = map (mapFirst f) . rec env g

-- | Turn a function (which returns its result in the Maybe monad) into a transformation
makeTrans :: (a -> Maybe a) -> Transformation a
makeTrans = makeTransG

makeRewriteTrans :: RewriteRule a -> Transformation a
makeRewriteTrans = transRewrite

-- | Turn a function (which returns a list of results) into a transformation
makeTransG ::  Foldable f => (a -> f a) -> Transformation a
makeTransG f = transList (toList . f)

makeTransEnv :: (a -> EnvMonad a) -> Transformation (Context a)
makeTransEnv f = ((split >>> BoxedEnv (transEnv f)) &&& C.id) >>> assemble
 where
   split    = transMaybe $ \c -> liftM (\a -> (a, environment c)) (current c)
   assemble = arr $ (\((b, env), c) -> setEnvironment env (replace b c))

makeTransEnv_ :: (a -> EnvMonad ()) -> Transformation (Context a)
makeTransEnv_ f = makeTransEnv (\a -> f a >> return a)

-----------------------------------------------------------
--- Bindables

type ParamTrans a b = Trans (a, b) b

supplyParameters :: ParamTrans b a -> (a -> Maybe b) -> Transformation a
supplyParameters f g = transMaybe g &&& C.id >>> f

supplyContextParameters :: ParamTrans b a -> (a -> EnvMonad b) -> Transformation (Context a)
supplyContextParameters f g = 
   (split >>> (BoxedEnv (transEnv g) &&& C.id) >>> arrange >>> first f) &&& C.id >>> assemble
 where
   split    = transMaybe $ \c -> liftM (\a -> (a, environment c)) (current c)
   arrange  = arr $ \((b, env), (a, _)) -> ((b, a), env)
   assemble = arr $ \((a, env), c) -> setEnvironment env (replace a c)
   
supplyLocals :: (a -> EnvMonad a) -> Transformation a
supplyLocals = transEnv

parameter1 :: (IsId n1, Reference a) => n1 -> (a -> Transformation b) -> ParamTrans a b
parameter1 n1 f = first (bindValue n1 >>> arr f) >>> app

parameter2 :: (IsId n1, IsId n2, Reference a, Reference b) 
           => n1 -> n2 -> (a -> b -> Transformation c) -> (ParamTrans (a, b) c)
parameter2 n1 n2 f = first (bindValue n1 *** bindValue n2 >>> arr (uncurry f)) >>> app

parameter3 :: (IsId n1, IsId n2, IsId n3, Reference a, Reference b, Reference c)
           => n1 -> n2 -> n3 -> (a -> b -> c -> Transformation d) -> (ParamTrans (a, b, c) d)
parameter3 n1 n2 n3 f = first ((\(a, b, c) -> (a, (b, c))) ^>> 
   bindValue n1 *** (bindValue n2 *** bindValue n3) >>^
   (\(a, (b, c)) -> f a b c)) 
           >>> app

-----------------------------------------------------------
--- Rules

transRewriteRules :: Trans a b -> [Some RewriteRule]
transRewriteRules trans =
   case trans of
      Rewrite r _ -> [Some r]
      BoxedEnv f  -> transRewriteRules f
      f :>>: g    -> transRewriteRules f ++ transRewriteRules g
      f :**: g    -> transRewriteRules f ++ transRewriteRules g
      f :++: g    -> transRewriteRules f ++ transRewriteRules g
      Append f g  -> transRewriteRules f ++ transRewriteRules g
      _           -> []
      
transRefs :: Trans a b -> IO [Some Ref]
transRefs trans = 
   case trans of
      Bind r      -> return [Some r]
      BoxedEnv f  -> transRefs f
      TransEnv f  -> envMonadFunctionRefs f
      f :>>: g    -> transRefs f ++++ transRefs g
      f :**: g    -> transRefs f ++++ transRefs g
      f :++: g    -> transRefs f ++++ transRefs g
      Append f g  -> transRefs f ++++ transRefs g
      _           -> return []
 where
   (++++) = liftM2 (++)