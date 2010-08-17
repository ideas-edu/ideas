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
   ( Symbol, newSymbol
   , Term(..), IsTerm(..)
   , fromTermM, fromTermWith
   , getSpine, getConSpine, makeTerm, makeConTerm
   , unary, binary, isUnary, isBinary, isVar, isCon
   , hasMetaVar, getMetaVars
   ) where

import Common.Id
import Common.Utils (ShowString(..))
import Common.Uniplate
import Control.Monad
import Data.List
import Data.Typeable

-----------------------------------------------------------
-- * Data type for terms

newtype Symbol = S { symbolId :: Id }
   deriving (Eq, Ord)

newSymbol :: String -> Symbol
newSymbol = S . newId

instance Show Symbol where
   show = showId

instance HasId Symbol where
   getId      = symbolId
   changeId f = S . f . symbolId 

data Term = Var   String 
          | Con   Symbol 
          | Apply [Term]
          | Num   Integer 
          | Float Double
          | Meta  Int
 deriving (Show, Eq, Ord, Typeable)
 
instance Uniplate Term where
   uniplate (Apply xs) = (xs, Apply)
   uniplate term       = ([], \_ -> term)

-----------------------------------------------------------
-- * Type class for conversion to/from terms

class IsTerm a where
   toTerm   :: a -> Term
   fromTerm :: MonadPlus m => Term -> m a

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

fromTermWith :: (Monad m, IsTerm a) => (Symbol -> [a] -> m a) -> Term -> m a
fromTermWith f a = 
   case getSpine a of 
      (t, xs) -> isCon t >>= \s -> mapM fromTermM xs >>= f s

-----------------------------------------------------------
-- * Utility functions

getSpine :: Term -> (Term, [Term])
getSpine (Apply (x:xs)) = (x, xs)
getSpine a = (a, [])

getConSpine :: Monad m => Term -> m (Symbol, [Term])
getConSpine a = liftM (\s -> (s, xs)) (isCon b)
 where (b, xs) = getSpine a

makeTerm :: Term -> [Term] -> Term
makeTerm x xs = Apply (x:xs)

makeConTerm :: Symbol -> [Term] -> Term
makeConTerm = makeTerm . Con

unary :: Symbol -> Term -> Term
unary s a = Apply [Con s, a]

binary :: Symbol -> Term -> Term -> Term
binary s a b = Apply [Con s, a, b]

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
isCon _       = fail "isCon"

getMetaVars :: Term -> [Int]
getMetaVars a = sort $ nub [ i | Meta i <- leafs a ]

hasMetaVar :: Int -> Term -> Bool
hasMetaVar i = (i `elem`) . getMetaVars