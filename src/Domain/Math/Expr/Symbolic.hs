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
-----------------------------------------------------------------------------
module Domain.Math.Expr.Symbolic 
   ( module Domain.Math.Expr.Symbolic, Symbol
   ) where

import Control.Monad
import Common.Rewriting.Term
import qualified Text.OpenMath.Symbol as OM

makeSymbol :: String -> String -> Symbol
makeSymbol = S . Just

class IsSymbol a where
   toSymbol   :: a -> Symbol
   fromSymbol :: Symbol -> a

instance IsSymbol Symbol where
   toSymbol   = id
   fromSymbol = id

instance IsSymbol String where
   toSymbol = S Nothing
   fromSymbol (S (Just a) b) = a ++ "." ++ b
   fromSymbol (S Nothing  b) = b

instance IsSymbol OM.Symbol where
   toSymbol s = S (OM.dictionary s) (OM.symbolName s) 
   fromSymbol (S (Just a) b) = OM.makeSymbol a b
   fromSymbol (S Nothing  b) = OM.extraSymbol b

stringToSymbol :: String -> Symbol
stringToSymbol s = 
   case break (=='.') s of
      (xs, _:ys) -> S (Just xs) ys
      _          -> S Nothing s

-------------------------------------------------------------------
-- Type class for symbolic representations

class Symbolic a where
   -- constructing
   variable   :: String -> a
   symbol     :: Symbol -> a
   function   :: Symbol -> [a] -> a
   -- matching
   getVariable :: MonadPlus m => a -> m String
   getSymbol   :: MonadPlus m => a -> m Symbol
   getFunction :: MonadPlus m => a -> m (Symbol, [a])
   isSymbol    :: MonadPlus m => Symbol -> a -> m [a]
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
   getVariable = isVar
   getSymbol   = isCon
   getFunction = getConSpine
   
nullary :: (IsSymbol s, Symbolic a) => s -> a
nullary = symbol . toSymbol
   
unary :: (IsSymbol s, Symbolic a) => s -> a -> a
unary f a = function (toSymbol f) [a]

binary :: (IsSymbol s, Symbolic a) => s -> a -> a -> a
binary f a b = function (toSymbol f) [a, b]

isConst :: (IsSymbol s, Symbolic a) => s -> a -> Bool
isConst s = maybe False null . isSymbol (toSymbol s) 

isUnary :: (IsSymbol s, Symbolic a, MonadPlus m) => s -> a -> m a
isUnary s a = 
   case isSymbol (toSymbol s) a of
      Just [x] -> return x
      _ -> mzero

isBinary :: (IsSymbol s, Symbolic a, MonadPlus m) => s -> a -> m (a, a)
isBinary s a = 
   case isSymbol (toSymbol s) a of
      Just [x, y] -> return (x, y)
      _ -> mzero

-- left-associative by default
isAssoBinary :: (IsSymbol s, Symbolic a, MonadPlus m) => s -> a -> m (a, a)
isAssoBinary s a =
   case isSymbol (toSymbol s) a of
      Just [x, y] -> return (x, y)
      Just (x:xs) | length xs > 1 -> return (x, function (toSymbol s) xs)
      _ -> mzero
      
fromTermWith :: (MonadPlus m, IsSymbol s, IsTerm a) 
             => (s -> [a] -> m a) -> Term -> m a
fromTermWith f term = do
   (s, xs) <- getFunction term
   ys <- mapM fromTermM xs
   f (fromSymbol s) ys