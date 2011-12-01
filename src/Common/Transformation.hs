{-# LANGUAGE ExistentialQuantification #-}
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
   , expectedEnvironment, getRewriteRules
     -- * QuickCheck generator
   , smartGen
     -- * Recognizer
   , Recognizer, makeRecognizer, makeListRecognizer, simpleRecognizer
   , recognize, recognizeList, buggyRecognizer, isBuggyRecognizer
   , changeEnvironment -- tmp
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
import Data.Maybe
import Data.Typeable
import Test.QuickCheck

-----------------------------------------------------------
--- Transformations

newtype Results a = R (Environment -> [(a, Environment)])

instance Monoid (Results a) where
   mempty = R $ const []
   R f `mappend` R g = R $ \env -> f env ++ g env

instance Functor Results where
   fmap = liftM

instance Monad Results where
   return a  = R $ \env -> [(a, env)]
   fail _    = mempty
   R f >>= g = R $ \old -> do
      (a, env) <- f old
      let R h = g a
      h env

instance MonadPlus Results where
   mzero = mempty
   mplus = mappend

getEnvironment :: Results Environment
getEnvironment = R $ \env -> [(env, env)]

changeEnvironment :: (Environment -> Environment) -> Results ()
changeEnvironment f = R $ return . (,) () . f

localBinding :: Typeable a => Binding a -> Results ()
localBinding = changeEnvironment . insertBinding

toResults :: Foldable f => f a -> Results a
toResults = mconcat . map return . toList

fromResults :: Results a -> [a]
fromResults (R f) = map fst (f mempty)

-- | Abstract data type for representing transformations
data Transformation a
   = Function (a -> Results a)
   | RewriteRule (RewriteRule a) (a -> Results a)
   | forall b c . LiftView (View a (b, c)) (Transformation b)
   | Transformation a :*: Transformation a
   | Transformation a :|: Transformation a

instance SemiRing (Transformation a) where
   zero  = makeTrans (const Nothing)
   (<+>) = (:|:)
   one   = makeTrans Just
   (<*>) = (:*:)

instance Apply Transformation where
   applyAll trans = fromResults . getResults trans
   
getResults :: Transformation a -> a -> Results a
getResults trans a =
   case trans of
      Function f      -> f a
      RewriteRule _ f -> f a
      LiftView v t    -> do (b0, c) <- matchM v a
                            b <- getResults t b0
                            return (build v (b, c))
      t1 :|: t2       -> getResults t1 a `mappend` getResults t2 a
      t1 :*: t2       -> getResults t1 a >>= getResults t2

instance LiftView Transformation where
   liftViewIn = LiftView

-- | Turn a function (which returns its result in the Maybe monad) into a transformation
makeTrans :: (a -> Maybe a) -> Transformation a
makeTrans = makeTransG

makeEnvTrans :: (a -> Results a) -> Transformation a
makeEnvTrans = Function

-- | Turn a function (which returns a list of results) into a transformation
makeTransG :: Foldable f => (a -> f a) -> Transformation a
makeTransG f = Function (toResults . f)

-----------------------------------------------------------
--- HasTransformation type class

class HasTransformation f where
   transformation :: f a -> Transformation a

instance HasTransformation Transformation where
   transformation = id

instance HasTransformation RewriteRule where
   transformation r = RewriteRule r (rewriteM r)

-----------------------------------------------------------
--- Bindables

-- | Parameterization with one Bindable using the provided label
supply1 :: Bindable x
                  => String -> (a -> Maybe x)
                  -> (x -> Transformation a) -> Transformation a
supply1 s f g = Function $ \a -> do
   x <- toResults (f a)
   localBinding (setValue x $ makeBinding s)
   getResults (g x) a

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

-- temporary solution
expectedEnvironment :: HasTransformation f => f a -> a -> Environment
expectedEnvironment f a = fromMaybe mempty $ listToMaybe $ fromResults $
   getResults (transformation f) a >> getEnvironment

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
         LiftView _ t      -> rec t
         t1 :|: t2         -> rec t1 ++ rec t2
         t1 :*: t2         -> rec t1 ++ rec t2

transRecognizer :: (IsId n, HasTransformation f)
                => (a -> a -> Bool) -> n -> f a -> Recognizer a
transRecognizer eq n f = makeListRecognizer n $ \a b -> fromResults $ do
   x <- getResults (transformation f) a
   guard (x `eq` b)
   getEnvironment

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