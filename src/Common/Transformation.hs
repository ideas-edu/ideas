{-# LANGUAGE ExistentialQuantification, TypeOperators #-}
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
module Common.Transformation
   ( -- * Transformations
     Transformation, liftViewInTrans, sequenceTrans
   , makeTrans, makeTransG, makeTransEnv, makeRewriteTrans, makeTransEnv_
   , applyTransformation
     -- * Bindables
   , ParamTrans, supplyLocals
   , supplyParameters, supplyContextParameters
     -- * Recognizers
   , transRecognizer
     -- * Extract information
   , transRewriteRules
     -- * Recognizer
   , Recognizer, makeRecognizer, makeListRecognizer, simpleRecognizer
   , recognize, recognizeList, buggyRecognizer, isBuggyRecognizer
   ) where

import Common.Environment
import Common.Classes
import Common.Context
import Common.Id
import Common.Navigator
import Common.Parameterized
import Common.Rewriting
import Common.Utils
import Common.View
import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.Monoid

-----------------------------------------------------------
--- Transformations

-- | Abstract data type for representing transformations
data Transformation a
   = Function (a -> [(a, Environment)])
   | RewriteRule (RewriteRule a) (a -> [a])
   | forall b c . LiftView (View a (b, c)) (Transformation b)
   | Transformation a :*: Transformation a
   | Transformation a :|: Transformation a

instance Monoid (Transformation a) where
   mempty  = makeTrans (const Nothing)
   mappend = (:|:)

applyTransformation :: Transformation a -> a -> [(a, Environment)]
applyTransformation trans a =
   case trans of
      Function f      -> f a
      RewriteRule _ f -> [ (b, mempty) | b <- f a ]
      LiftView v t    -> do (b0, c) <- matchM v a
                            (b, env) <- applyTransformation t b0
                            return (build v (b, c), env)
      t1 :|: t2       -> applyTransformation t1 a ++ applyTransformation t2 a
      t1 :*: t2       -> do 
         (b, env1) <- applyTransformation t1 a
         (c, env2) <- applyTransformation t2 b
         return (c, env2 `mappend` env1)

liftViewInTrans :: View a (b, c) -> Transformation b -> Transformation a
liftViewInTrans = LiftView

sequenceTrans :: Transformation a -> Transformation a -> Transformation a
sequenceTrans = (:*:)

-- | Turn a function (which returns its result in the Maybe monad) into a transformation
makeTrans :: (a -> Maybe a) -> Transformation a
makeTrans = makeTransG

makeRewriteTrans :: RewriteRule a -> Transformation a
makeRewriteTrans r = RewriteRule r (applyAll r)

-- | Turn a function (which returns a list of results) into a transformation
makeTransG ::  Foldable f => (a -> f a) -> Transformation a
makeTransG f = Function $ \a -> [ (b, mempty) | b <- toList (f a) ]

makeTransEnv :: (a -> EnvMonad a) -> Transformation (Context a)
makeTransEnv f = makeTrans $ \ca -> do
   a <- current ca
   (b, env) <- runEnvMonad (f a) (environment ca)
   return (setEnvironment env (replace b ca))

makeTransEnv_ :: (a -> EnvMonad ()) -> Transformation (Context a)
makeTransEnv_ f = makeTransEnv (\a -> f a >> return a)

-----------------------------------------------------------
--- Bindables

type ParamTrans b a = b :>-> Transformation a

supplyParameters :: ParamTrans b a -> (a -> Maybe b) -> Transformation a
supplyParameters f g = Function $ \a -> do
   b <- maybeToList (g a)
   let (trans, env1) = annotatedFunction f b
   (c, env2) <- applyTransformation trans a
   return (c, env2 `mappend` env1)

supplyContextParameters :: ParamTrans b a -> (a -> EnvMonad b) -> Transformation (Context a)
supplyContextParameters f g = supplyParameters newf newg 
 where
   newf   = fmap (LiftView contextView) f
   newg c = current c >>= \a -> evalEnvMonad (g a) (environment c)

supplyLocals :: (a -> EnvMonad a) -> Transformation a
supplyLocals f = Function $ toList . flip runEnvMonad mempty . f

-----------------------------------------------------------
--- Rules

transRewriteRules :: Transformation a -> [Some RewriteRule]
transRewriteRules trans =
   case trans of
      Function _        -> []
      RewriteRule rr _  -> [Some rr]
      LiftView _ t      -> transRewriteRules t
      t1 :|: t2         -> transRewriteRules t1 ++ transRewriteRules t2
      t1 :*: t2         -> transRewriteRules t1 ++ transRewriteRules t2

transRecognizer :: IsId n => (a -> a -> Bool) -> n -> Transformation a -> Recognizer a
transRecognizer eq n f = makeListRecognizer n $ \a b -> do
   (x, env) <- applyTransformation f a
   guard (x `eq` b)
   return env
            
-----------------------------------------------------------
--- Recognizer

data Recognizer a = Recognizer 
   { recognizerId      :: Id
   , recognizeList     :: a -> a -> [Environment]
   , isBuggyRecognizer :: Bool
   }

instance HasId (Recognizer a) where
   getId = recognizerId 
   changeId f r = r {recognizerId = f (recognizerId r)}

instance LiftView Recognizer where
   liftViewIn v r = r {recognizeList = make}
    where
      make a b = do
         (x, _) <- matchM v a
         (y, _) <- matchM v b
         recognizeList r x y

makeRecognizer :: IsId n => n -> (a -> a -> Maybe Environment) -> Recognizer a
makeRecognizer n f = makeListRecognizer n (\a b -> maybeToList $ f a b)

makeListRecognizer :: IsId n => n -> (a -> a -> [Environment]) -> Recognizer a
makeListRecognizer n f = Recognizer (newId n) f False

simpleRecognizer :: IsId n => n -> (a -> a -> Bool) -> Recognizer a
simpleRecognizer n eq = makeRecognizer n $ \a b ->
   guard (eq a b) >> return mempty

recognize :: Recognizer a -> a -> a -> Maybe Environment
recognize r a b = listToMaybe $ recognizeList r a b 

buggyRecognizer :: Recognizer a -> Recognizer a
buggyRecognizer r = r {isBuggyRecognizer = True}