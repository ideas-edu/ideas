{-# LANGUAGE TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
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
module Common.Rewriting.Term where

import Common.Utils (ShowString(..))
import Common.Uniplate
import Control.Monad
import Common.Rewriting.MetaVar

-----------------------------------------------------------
-- * Data type for terms

data Symbol = S (Maybe String) String
   deriving (Eq, Ord)

data Term = Var  String 
          | Con  Symbol 
          | App  Term Term
          | Num  Integer 
          | Meta Int
 deriving (Show, Eq, Ord)

instance Show Symbol where
   show (S ma b) = maybe b (\a -> a++ "." ++ b) ma

instance MetaVar Term where 
   metaVar = Meta
   isMetaVar (Meta n) = Just n
   isMetaVar _ = Nothing
 
instance Uniplate Term where
   uniplate (App f a) = ([f,a], \[g,b] -> App g b)
   uniplate term      = ([], \_ -> term)

-----------------------------------------------------------
-- * Type class for conversion to/from terms

class IsTerm a where
   toTerm   :: a -> Term
   fromTerm :: Term -> Maybe a

instance IsTerm Term where
   toTerm   = id
   fromTerm = return

instance IsTerm ShowString where 
   toTerm = Var . fromShowString
   fromTerm (Var s) = return (ShowString s)
   fromTerm _       = Nothing

instance (IsTerm a, IsTerm b) => IsTerm (Either a b) where
   toTerm = either toTerm toTerm
   fromTerm expr =
      liftM Left  (fromTerm expr) `mplus`
      liftM Right (fromTerm expr) 

fromTermM :: (Monad m, IsTerm a) => Term -> m a
fromTermM = maybe (fail "fromTermM") return . fromTerm

fromTermWith :: (Monad m, IsTerm a) => (Symbol -> [a] -> m a) -> Term -> m a
fromTermWith f a = 
   case getSpine a of 
      (t, xs) -> isCon t >>= \s -> mapM fromTermM xs >>= f s

-----------------------------------------------------------
-- * Utility functions

getSpine :: Term -> (Term, [Term])
getSpine = rec []
 where
   rec xs (App f a) = rec (a:xs) f
   rec xs a         = (a, xs)

getConSpine :: Monad m => Term -> m (Symbol, [Term])
getConSpine a = liftM (\s -> (s, xs)) (isCon b)
 where (b, xs) = getSpine a

makeTerm :: Term -> [Term] -> Term
makeTerm = foldl App

makeConTerm :: Symbol -> [Term] -> Term
makeConTerm = makeTerm . Con

unary :: Symbol -> Term -> Term
unary = App . Con

binary :: Symbol -> Term -> Term -> Term
binary s = App . App (Con s)

isUnary :: Symbol -> Term -> Maybe Term
isUnary s term =
   case getSpine term of
      (t, [a]) | isCon t == Just s -> Just a
      _ -> Nothing

isBinary :: Symbol -> Term -> Maybe (Term, Term)
isBinary s term =
   case getSpine term of
      (t, [a, b]) | isCon t == Just s -> Just (a, b)
      _ -> Nothing

isVar :: Monad m => Term -> m String
isVar (Var s) = return s
isVar _       = fail "isVar"

isCon :: Monad m => Term -> m Symbol
isCon (Con s) = return s
isCon _        = fail "isCon"