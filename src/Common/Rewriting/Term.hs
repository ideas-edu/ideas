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
   , unary, unarySymbol, binarySymbol, binary, unaryMatch, binaryMatch
   , hasMetaVar, getMetaVars
   ) where

import Common.Id
import Common.Rewriting.Operator
import Common.Utils (ShowString(..))
import Common.Uniplate
import Common.View
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
   
instance IsId Symbol where
   newId = symbolId

data Term = Var   String 
          | Con   Symbol 
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

fromTermWith :: (Monad m, IsTerm a) => (Symbol -> [a] -> m a) -> Term -> m a
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

getConSpine :: Monad m => Term -> m (Symbol, [Term])
getConSpine a = 
   case getSpine a of
      (Con s, xs) -> return (s, xs)
      _           -> fail "getConSpine" 

makeTerm :: Term -> [Term] -> Term
makeTerm = foldl Apply

makeConTerm :: Symbol -> [Term] -> Term
makeConTerm = makeTerm . Con

unarySymbol :: Symbol -> UnaryOp Term
unarySymbol s = makeUnary (getId s) (Apply (Con s)) f
 where
   f (Apply (Con t) a) | s==t = Just a
   f _ = Nothing

binarySymbol :: Symbol -> BinaryOp Term
binarySymbol s = makeBinary (getId s) (Apply . Apply (Con s)) f
 where
   f (Apply (Apply (Con t) a) b) | s==t = Just (a, b)
   f _ = Nothing

getMetaVars :: Term -> [Int]
getMetaVars a = sort $ nub [ i | Meta i <- leafs a ]

hasMetaVar :: Int -> Term -> Bool
hasMetaVar i = (i `elem`) . getMetaVars