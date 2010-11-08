{-# LANGUAGE TypeSynonymInstances #-}
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
-----------------------------------------------------------------------------
module Domain.Math.Expr.Symbolic 
   ( isBinary, Symbolic, binary, isUnary
   , isAssoBinary, fromTermWith, nullary, unary 
   , function, getFunction, getVariable, variable, symbol
   , isVariable) where

import Common.Id
import Control.Monad
import Data.Maybe
import Data.Monoid
import Common.Rewriting.Term hiding (fromTermWith)
import qualified Text.OpenMath.Symbol as OM

instance IsId OM.Symbol where
   newId s = fromMaybe mempty (OM.dictionary s) # OM.symbolName s

-------------------------------------------------------------------
-- Type class for symbolic representations

class Symbolic a where
   -- constructing
   variable   :: String -> a
   symbol     :: Id -> a
   function   :: Id -> [a] -> a
   -- matching
   getVariable :: MonadPlus m => a -> m String
   getSymbol   :: MonadPlus m => a -> m Id
   getFunction :: MonadPlus m => a -> m (Id, [a])
   isSymbol    :: MonadPlus m => Id -> a -> m [a]
   -- default definition
   symbol s = function s []
   getSymbol a = do
      (t, as) <- getFunction a 
      guard (null as)
      return t
   isSymbol s a = do
      (t, as) <- getFunction a
      guard (s==t)
      return as

instance Symbolic Term where 
   variable    = Var
   symbol      = Con
   function    = makeConTerm
   getFunction = getConSpine
   getVariable (Var s) = return s
   getVariable _       = fail "isVar"
   getSymbol   (Con s) = return s
   getSymbol   _       = fail "isCon"

nullary :: (IsId s, Symbolic a) => s -> a
nullary = symbol . newId
   
unary :: (IsId s, Symbolic a) => s -> a -> a
unary f a = function (newId f) [a]

binary :: (IsId s, Symbolic a) => s -> a -> a -> a
binary f a b = function (newId f) [a, b]

binaryA :: (IsId s, Symbolic a) => s -> a -> a -> a
binaryA f a b = function fs (collect a ++ collect b)
 where
   fs = newId f
   collect t =
      case getFunction t of
         Just (s, xs) | fs==s -> xs
         _ -> [t]

isConst :: (IsId s, Symbolic a) => s -> a -> Bool
isConst s = maybe False null . isSymbol (newId s) 

isVariable :: Symbolic a => a -> Bool
isVariable = isJust . getVariable

isUnary :: (IsId s, Symbolic a, MonadPlus m) => s -> a -> m a
isUnary s a = 
   case isSymbol (newId s) a of
      Just [x] -> return x
      _ -> mzero

isBinary :: (IsId s, Symbolic a, MonadPlus m) => s -> a -> m (a, a)
isBinary s a = 
   case isSymbol (newId s) a of
      Just [x, y] -> return (x, y)
      _ -> mzero

-- left-associative by default
isAssoBinary :: (IsId s, Symbolic a, MonadPlus m) => s -> a -> m (a, a)
isAssoBinary s a =
   case isSymbol (newId s) a of
      Just [x, y] -> return (x, y)
      Just (x:xs) | length xs > 1 -> return (x, function (newId s) xs)
      _ -> mzero
      
fromTermWith :: (MonadPlus m, IsTerm a) 
             => (Id -> [a] -> m a) -> Term -> m a
fromTermWith f term = do
   (s, xs) <- getConSpine term
   ys <- mapM fromTermM xs
   f s ys