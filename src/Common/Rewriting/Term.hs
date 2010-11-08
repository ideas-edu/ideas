{-# LANGUAGE TypeSynonymInstances, DeriveDataTypeable #-}
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
-- A simple data type for term rewriting
--
-----------------------------------------------------------------------------
module Common.Rewriting.Term 
   ( Term(..), IsTerm(..)
   , fromTermM, fromTermWith
   , getSpine, getConSpine, makeTerm, makeConTerm
   , constantTerm, unaryTerm, binaryTerm
   , hasMetaVar, getMetaVars
   ) where

import Common.Id
import Common.Utils (ShowString(..))
import Common.Uniplate
import Common.View
import Control.Monad
import Data.List
import Data.Typeable

-----------------------------------------------------------
-- * Data type for terms

data Term = Var   String 
          | Con   Id 
          | Apply Term Term
          | Num   Integer 
          | Float Double
          | Meta  Int
 deriving (Show, Eq, Ord, Typeable)
 
instance Uniplate Term where
   uniplate (Apply f a) = ([f, a], \[g, b] -> Apply g b)
   uniplate term        = ([], \_ -> term)

-----------------------------------------------------------
-- * Type class for conversion to/from terms

class IsTerm a where
   toTerm   :: a -> Term
   fromTerm :: MonadPlus m => Term -> m a
   termView :: View Term a
   -- default definitions
   toTerm   = build termView
   fromTerm = matchM termView
   termView = makeView fromTerm toTerm

instance IsTerm Term where
   toTerm   = id
   fromTerm = return

instance IsTerm ShowString where 
   toTerm = Var . fromShowString
   fromTerm (Var s) = return (ShowString s)
   fromTerm _       = fail "fromTerm"

instance (IsTerm a, IsTerm b) => IsTerm (Either a b) where
   toTerm = either toTerm toTerm
   fromTerm expr =
      liftM Left  (fromTerm expr) `mplus`
      liftM Right (fromTerm expr) 

fromTermM :: (Monad m, IsTerm a) => Term -> m a
fromTermM = maybe (fail "fromTermM") return . fromTerm

fromTermWith :: (Monad m, IsTerm a) => (Id -> [a] -> m a) -> Term -> m a
fromTermWith f a = do
   (s, xs) <- getConSpine a
   ys <-  mapM fromTermM xs
   f s ys

-----------------------------------------------------------
-- * Utility functions

getSpine :: Term -> (Term, [Term])
getSpine = rec [] 
 where
   rec xs (Apply f a) = rec (a:xs) f
   rec xs a           = (a, xs)

getConSpine :: Monad m => Term -> m (Id, [Term])
getConSpine a = 
   case getSpine a of
      (Con s, xs) -> return (s, xs)
      _           -> fail "getConSpine" 

makeTerm :: Term -> [Term] -> Term
makeTerm = foldl Apply

makeConTerm :: IsId a => a -> [Term] -> Term
makeConTerm = makeTerm . constantTerm

constantTerm :: IsId a => a -> Term
constantTerm = Con . newId

unaryTerm :: IsId a => a -> Term -> Term
unaryTerm = Apply . constantTerm

binaryTerm :: IsId a => a -> Term -> Term -> Term
binaryTerm = (Apply .) . unaryTerm

getMetaVars :: Term -> [Int]
getMetaVars a = sort $ nub [ i | Meta i <- leafs a ]

hasMetaVar :: Int -> Term -> Bool
hasMetaVar i = (i `elem`) . getMetaVars