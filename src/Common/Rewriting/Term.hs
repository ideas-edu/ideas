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
   , getSpine, makeTerm
     -- * Functions and symbols
   , WithFunctions(..), isSymbol, isFunction
   , unary, binary, isUnary, isBinary
     -- * Variables
   , WithVars(..), isVariable
   , vars, varSet, hasVar, withoutVar, hasSomeVar, hasNoVar
     -- * Meta variables
   , WithMetaVars(..), isMetaVar
   , metaVars, metaVarSet, hasMetaVar
   ) where

import Common.Id
import Common.Utils (ShowString(..))
import Common.Uniplate
import Common.View
import Control.Monad
import Data.Maybe
import Data.Typeable
import qualified Data.IntSet as IS
import qualified Data.Set as S

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
   (s, xs) <- getFunction a
   ys      <- mapM fromTermM xs
   f s ys

-----------------------------------------------------------
-- * Functions and symbols

class WithFunctions a where
   -- constructing
   symbol   :: IsId s => s -> a
   function :: IsId s => s -> [a] -> a
   -- matching
   getSymbol   :: Monad m => a -> m Id
   getFunction :: Monad m => a -> m (Id, [a])
   -- default definition
   symbol s = function s []
   getSymbol a = 
      case getFunction a of
         Just (t, []) -> return t
         _            -> fail "Common.Term.getSymbol"
         
instance WithFunctions Term where
   function    = makeTerm . Con . newId
   getFunction a = 
      case getSpine a of
         (Con s, xs) -> return (s, xs)
         _           -> fail "Common.Rewriting.getFunction" 
   
isSymbol :: (IsId s, WithFunctions a) => s -> a -> Bool
isSymbol s = maybe False (sameId s) . getSymbol

isFunction :: (IsId s, WithFunctions a, Monad m) => s -> a -> m [a]
isFunction s a =
   case getFunction a of
      Just (t, as) | sameId s t -> return as
      _                         -> fail "Common.Term.isFunction"

unary :: (IsId s, WithFunctions a) => s -> a -> a
unary s a = function s [a]

binary :: (IsId s, WithFunctions a) => s -> a -> a -> a
binary s a b = function s [a, b]

isUnary :: (IsId s, WithFunctions a, Monad m) => s -> a -> m a
isUnary s a = 
   case isFunction s a of
      Just [x] -> return x
      _        -> fail "Common.Term.isUnary"

isBinary :: (IsId s, WithFunctions a, Monad m) => s -> a -> m (a, a)
isBinary s a = 
   case isFunction s a of
      Just [x, y] -> return (x, y)
      _           -> fail "Common.Term.isBinary"

-----------------------------------------------------------
-- * Variables

class WithVars a where
   variable    :: String -> a
   getVariable :: Monad m => a -> m String 

instance WithVars Term where 
   variable    = Var
   getVariable (Var s) = return s
   getVariable _       = fail "Common.Rewriting.getVariable"

isVariable :: WithVars a => a -> Bool
isVariable = isJust . getVariable

vars :: (Uniplate a, WithVars a) => a -> [String]
vars = concatMap getVariable . leafs

varSet :: (Uniplate a, WithVars a) => a -> S.Set String
varSet = S.fromList . vars

hasVar :: (Uniplate a, WithVars a) => String -> a -> Bool
hasVar i = (i `elem`) . vars

withoutVar :: (Uniplate a, WithVars a) => String -> a -> Bool
withoutVar i = not . hasVar i

hasSomeVar :: (Uniplate a, WithVars a) => a -> Bool
hasSomeVar = not . hasNoVar

hasNoVar :: (Uniplate a, WithVars a) => a -> Bool
hasNoVar = null . vars

-----------------------------------------------------------
-- * Meta variables

class WithMetaVars a where
   metaVar    :: Int -> a
   getMetaVar :: Monad m => a -> m Int 

instance WithMetaVars Term where
   metaVar = Meta
   getMetaVar (Meta i) = return i
   getMetaVar _        = fail "Common.Rewriting.getMetaVar"

isMetaVar :: WithMetaVars a => a -> Bool
isMetaVar = isJust . getMetaVar

metaVars :: (Uniplate a, WithMetaVars a) => a -> [Int]
metaVars = concatMap getMetaVar . leafs

metaVarSet :: (Uniplate a, WithMetaVars a) => a -> IS.IntSet
metaVarSet = IS.fromList . metaVars

hasMetaVar :: (Uniplate a, WithMetaVars a) => Int -> a -> Bool
hasMetaVar i = (i `elem`) . metaVars

-----------------------------------------------------------
-- * Utility functions

getSpine :: Term -> (Term, [Term])
getSpine = rec [] 
 where
   rec xs (Apply f a) = rec (a:xs) f
   rec xs a           = (a, xs)

makeTerm :: Term -> [Term] -> Term
makeTerm = foldl Apply