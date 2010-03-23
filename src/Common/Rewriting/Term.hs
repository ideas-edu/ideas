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

import Common.Uniplate
import Common.Rewriting.MetaVar

-----------------------------------------------------------
-- * Data type for terms

data Term = Var  String 
          | Con  String 
          | App  Term Term
          | Num  Integer 
          | Meta Int
 deriving (Show, Eq, Ord)

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
   toTerm :: a -> Term
   fromTerm :: Term -> Maybe a

instance IsTerm String where
   toTerm = Var
   fromTerm (Var s) = return s
   fromTerm _ = Nothing

fromTermM :: (Monad m, IsTerm a) => Term -> m a
fromTermM = maybe (fail "fromTermM") return . fromTerm

-----------------------------------------------------------
-- * Utility functions

getSpine :: Term -> (Term, [Term])
getSpine = rec []
 where
   rec xs (App f a) = rec (a:xs) f
   rec xs a         = (a, xs)

makeTerm :: Term -> [Term] -> Term
makeTerm = foldl App

makeConTerm :: String -> [Term] -> Term
makeConTerm = makeTerm . Con

unary :: String -> Term -> Term
unary = App . Con

binary :: String -> Term -> Term -> Term
binary s = App . App (Con s)

isUnary :: String -> Term -> Maybe Term
isUnary s term =
   case getSpine term of
      (Con t, [a]) | s==t -> Just a
      _ -> Nothing

isBinary :: String -> Term -> Maybe (Term, Term)
isBinary s term =
   case getSpine term of
      (Con t, [a, b]) | s==t -> Just (a, b)
      _ -> Nothing
      
isCon :: Term -> Maybe String
isCon (Con s) = Just s
isCon _       = Nothing