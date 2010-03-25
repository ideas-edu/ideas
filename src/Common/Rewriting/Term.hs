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
import qualified Text.OpenMath.Symbol as OM

data Symbol = S (Maybe String) String
   deriving (Eq, Ord)

instance Show Symbol where
   show (S ma b) = maybe b (\a -> a++ "." ++ b) ma

class IsSymbol a where
   toSymbol :: a -> Symbol

instance IsSymbol Symbol where
   toSymbol = id

instance IsSymbol String where
   toSymbol = S Nothing

instance IsSymbol OM.Symbol where
   toSymbol s = S (OM.dictionary s) (OM.symbolName s) 
   
makeSymbol :: String -> String -> Symbol
makeSymbol = S . Just

fromSymbol :: Symbol -> OM.Symbol
fromSymbol (S ma b) = maybe (OM.extraSymbol b) (\a -> OM.makeSymbol a b) ma

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

makeConTerm :: IsSymbol s => s -> [Term] -> Term
makeConTerm = makeTerm . con

unary :: IsSymbol s => s -> Term -> Term
unary = App . con

binary :: IsSymbol s => s -> Term -> Term -> Term
binary s = App . App (con s)

isUnary :: IsSymbol s => s -> Term -> Maybe Term
isUnary s term =
   case getSpine term of
      (t, [a]) | isCon t == Just (toSymbol s) -> Just a
      _ -> Nothing

isBinary :: IsSymbol s => s -> Term -> Maybe (Term, Term)
isBinary s term =
   case getSpine term of
      (t, [a, b]) | isCon t == Just (toSymbol s) -> Just (a, b)
      _ -> Nothing
      
con :: IsSymbol s => s  -> Term
con = Con . toSymbol

isCon :: Monad m => Term -> m Symbol
isCon (Con s) = return s
isCon _        = fail "isCon"