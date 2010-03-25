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
module Common.Rewriting.Term 
   ( module Common.Rewriting.Term, Symbol, makeSymbol ) where

import Common.Utils (ShowString(..))
import Common.Uniplate
import Control.Monad
import Common.Rewriting.MetaVar
import Text.OpenMath.Symbol -- to be removed

-----------------------------------------------------------
-- * Data type for terms

data Term = Var  String 
          | Con  Symbol 
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

instance IsTerm ShowString where 
   toTerm   = toTerm . fromShowString
   fromTerm = liftM ShowString . fromTerm

fromTermM :: (Monad m, IsTerm a) => Term -> m a
fromTermM = maybe (fail "fromTermM") return . fromTerm

fromTermWith :: (Monad m, IsTerm a) => (Symbol -> [a] -> m a) -> Term -> m a
fromTermWith f a = 
   case getSpine a of 
      (t, xs) -> isCon t >>= \s -> mapM fromTermM xs >>= f s
      _ -> fail "fromTermWith"

-----------------------------------------------------------
-- * Utility functions

getSpine :: Term -> (Term, [Term])
getSpine = rec []
 where
   rec xs (App f a) = rec (a:xs) f
   rec xs a         = (a, xs)

getConSpine :: Term -> Maybe (Symbol, [Term])
getConSpine a = fmap (\s -> (s, xs)) (isCon b)
 where (b, xs) = getSpine a


makeTerm :: Term -> [Term] -> Term
makeTerm = foldl App

makeConTerm :: Symbol -> [Term] -> Term
makeConTerm = makeTerm . con

unary :: Symbol -> Term -> Term
unary = App . con

binary :: Symbol -> Term -> Term -> Term
binary s = App . App (con s)

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
      
con :: Symbol -> Term
con = Con

isCon :: Monad m => Term -> m Symbol
isCon (Con s) = return s
isCon _        = fail "isCon"