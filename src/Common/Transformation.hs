{-# LANGUAGE GADTs, ExistentialQuantification #-}
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
     Transformation, HasTransformation(..)
   , makeTrans, makeEnvTrans, makeTransG
     -- * Bindables
   , supply1, supply2, supply3
     -- * Recognizers
   , transRecognizer
     -- * Extract information
   , getParameters, expectedEnvironment, getRewriteRules
     -- * QuickCheck generator
   , smartGen
     -- * Recognizer
   , Recognizer, makeRecognizer, simpleRecognizer
   , recognize, buggyRecognizer, isBuggyRecognizer
   ) where

import Common.Algebra.Field
import Common.Binding
import Common.Classes
import Common.Id
import Common.Rewriting
import Common.Utils
import Common.View
import Control.Monad
import Data.Foldable (Foldable, toList)
import Data.Monoid
import Data.Typeable
import Data.Maybe
import Test.QuickCheck

-----------------------------------------------------------
--- Transformations

type Results a = [(a, Environment)]

-- | Abstract data type for representing transformations
data Transformation a where
   Function    :: (a -> Results a) -> Transformation a
   RewriteRule :: RewriteRule a -> (a -> Results a) -> Transformation a
   Abstraction :: Typeable b => Binding b -> (a -> Maybe b) -> (b -> Transformation a) -> Transformation a
   LiftView    :: View a (b, c) -> Transformation b -> Transformation a
   (:|:)       :: Transformation a -> Transformation a -> Transformation a
   (:*:)       :: Transformation a -> Transformation a -> Transformation a

instance SemiRing (Transformation a) where
   zero  = makeTrans (const Nothing)
   (<+>) = (:|:)
   one   = makeTrans Just
   (<*>) = (:*:)

instance Apply Transformation where
   applyAll trans = map fst . getResults trans
   
getResults :: Transformation a -> a -> Results a
getResults trans a =
   case trans of
      Function f        -> f a
      RewriteRule _ f   -> f a
      Abstraction _ f g -> maybe [] (\b -> getResults (g b) a) (f a)
      LiftView v t      -> [ (build v (b, c), xs) | (b0, c) <- matchM v a, (b, xs) <- getResults t b0  ]
      t1 :|: t2         -> getResults t1 a ++ getResults t2 a
      t1 :*: t2         -> [ (c, xs `mappend` ys) | (b, xs) <- getResults t1 a, (c, ys) <- getResults t2 b ]

instance LiftView Transformation where
   liftViewIn = LiftView

-- | Turn a function (which returns its result in the Maybe monad) into a transformation
makeTrans :: (a -> Maybe a) -> Transformation a
makeTrans = makeTransG

makeEnvTrans :: (a -> Maybe (a, Environment)) -> Transformation a
makeEnvTrans f = Function (toList . f)

-- | Turn a function (which returns a list of results) into a transformation
makeTransG :: Foldable f => (a -> f a) -> Transformation a
makeTransG f = Function (toResults . toList . f)

toResults :: [a] -> Results a
toResults = map (\a -> (a, mempty))

-----------------------------------------------------------
--- HasTransformation type class

class HasTransformation f where
   transformation :: f a -> Transformation a

instance HasTransformation Transformation where
   transformation = id

instance HasTransformation RewriteRule where
   transformation r = RewriteRule r (toResults . rewriteM r)

-----------------------------------------------------------
--- Bindables

-- | Parameterization with one Bindable using the provided label
supply1 :: Bindable x
                  => String -> (a -> Maybe x)
                  -> (x -> Transformation a) -> Transformation a
supply1 = Abstraction . makeBinding

-- | Parameterization with two Bindables using the provided labels
supply2 :: (Bindable x, Bindable y)
                   => (String, String) -> (a -> Maybe (x, y))
                   -> (x -> y -> Transformation a) -> Transformation a
supply2 (s1, s2) f t =
   supply1 s1 (fmap fst . f) $ \x ->
   supply1 s2 (fmap snd . f) $ t x

-- | Parameterization with three Bindables using the provided labels
supply3 :: (Bindable x, Bindable y, Bindable z)
                  => (String, String, String) -> (a -> Maybe (x, y, z))
                  -> (x -> y -> z -> Transformation a) -> Transformation a
supply3 (s1, s2, s3) f t =
   supply1 s1 (fmap fst3 . f) $ \x -> 
   supply1 s2 (fmap snd3 . f) $ \y -> 
   supply1 s3 (fmap thd3 . f) $ t x y

-- | Returns a list of rule parameters
getParameters :: HasTransformation f => f a -> Environment
getParameters = rec . transformation 
 where
   rec :: Transformation a -> Environment
   rec trans =
      case trans of
         Function _          -> mempty
         RewriteRule _ _     -> mempty
         Abstraction env _ t -> insertBinding env (rec (t (getValue env)))
         LiftView _ t        -> rec t
         t1 :|: t2           -> rec t1 `mappend` rec t2
         t1 :*: t2           -> rec t1 `mappend` rec t2

-- | Returns a list of pretty-printed expected Bindables.
-- Nothing indicates that there are no such Bindables (or the Bindables
-- are not applicable for the current value)
expectedEnvironment :: HasTransformation f => f a -> a -> Environment
expectedEnvironment = rec . transformation
 where
   rec :: Transformation a -> a -> Environment
   rec trans a = 
      case trans of
         Function _      -> mempty
         RewriteRule _ _ -> mempty
         Abstraction env f t -> 
            case f a of
               Just b  -> insertBinding (setValue b env) (rec (t b) a)
               Nothing -> mempty
         LiftView v t -> 
            case match v a of
               Just (b, _) -> rec t b
               Nothing     -> mempty
         t1 :|: t2      -> rec t1 a `mappend` rec t2 a
         t1 :*: t2      -> rec t1 a `mappend` rec t2 a

{-
-- | Transform a rule and use a list of pretty-printed Bindables. Nothing indicates that the Bindables are
-- invalid (not parsable), or that the wrong number of Bindables was supplied
useBindablesTrans :: [String] -> Transformation a -> Maybe (Transformation a)
useBindablesTrans list = rec
 where
   rec :: Transformation a -> Maybe (Transformation a)
   rec trans =
      case trans of
         Function _           -> Nothing
         RewriteRule _ _      -> Nothing
         Abstraction env _ g -> case list of
                                    [hd] -> fmap g (parseBindable env hd)
                                    _    -> Nothing
         LiftView v t         -> fmap (LiftView v) (rec t)
         Recognizer f t       -> fmap (Recognizer f) (rec t)
         Choice t1 t2         -> rec t1 `mplus` rec t2
-}
-----------------------------------------------------------
--- Rules

getRewriteRules :: HasTransformation f => f a -> [Some RewriteRule]
getRewriteRules = rec . transformation 
 where
   rec :: Transformation a -> [Some RewriteRule]
   rec trans =
      case trans of
         Function _        -> []
         RewriteRule rr _  -> [Some rr]
         Abstraction _ _ _ -> []
         LiftView _ t      -> rec t
         t1 :|: t2         -> rec t1 ++ rec t2
         t1 :*: t2         -> rec t1 ++ rec t2

transRecognizer :: (IsId n, HasTransformation f)
                => (a -> a -> Bool) -> n -> f a -> Recognizer a
transRecognizer eq n f = makeRecognizer n $ \a b -> listToMaybe 
   [ env | (x, env) <- getResults (transformation f) a, x `eq` b ]

-----------------------------------------------------------
--- QuickCheck

smartGen :: HasTransformation f => f a -> a -> Maybe (Gen a)
smartGen = flip rec . transformation
 where
   rec :: a -> Transformation a -> Maybe (Gen a)
   rec a trans = 
      case trans of
         RewriteRule r _ -> return (smartGenerator r)
         LiftView v t -> do
            (b, c) <- matchM v a
            gen    <- rec b t
            return $ liftM (\n -> build v (n, c)) gen
         t1 :|: t2 -> recs [t1, t2]
         t1 :*: t2 -> recs [t1, t2]
         _ -> Nothing
    where
      recs ts =
         case mapMaybe (rec a) ts of
            [] -> Nothing
            xs -> return (oneof xs)
            
-----------------------------------------------------------
--- Recognizer

data Recognizer a = R 
   { recognizerId      :: Id
   , recognize         :: a -> a -> Maybe Environment
   , isBuggyRecognizer :: Bool
   }

instance HasId (Recognizer a) where
   getId = recognizerId 
   changeId f r = r {recognizerId = f (recognizerId r)}

instance LiftView Recognizer where
   liftViewIn v r = r {recognize = make}
    where
      make a b = do
         (x, _) <- match v a
         (y, _) <- match v b
         recognize r x y

makeRecognizer :: IsId n => n -> (a -> a -> Maybe Environment) -> Recognizer a
makeRecognizer n f = R (newId n) f False

simpleRecognizer :: IsId n => n -> (a -> a -> Bool) -> Recognizer a
simpleRecognizer n eq = makeRecognizer n $ \a b ->
   guard (eq a b) >> return mempty

buggyRecognizer :: Recognizer a -> Recognizer a
buggyRecognizer r = r {isBuggyRecognizer = True}