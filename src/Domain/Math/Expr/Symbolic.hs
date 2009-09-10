module Domain.Math.Expr.Symbolic 
   ( Symbol(..)
   , module Domain.Math.Expr.Symbolic
   ) where

import Control.Monad
import Text.OpenMath.Symbol

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
   
unary :: Symbolic a => Symbol -> a -> a
unary f a = function f [a]

binary :: Symbolic a => Symbol -> a -> a -> a
binary f a b = function f [a, b]

isConst :: Symbolic a => Symbol -> a -> Bool
isConst s = maybe False null . isSymbol s 

isUnary :: (Symbolic a, MonadPlus m) => Symbol -> a -> m a
isUnary s a = 
   case isSymbol s a of
      Just [x] -> return x
      _ -> mzero

isBinary :: (Symbolic a, MonadPlus m) => Symbol -> a -> m (a, a)
isBinary s a = 
   case isSymbol s a of
      Just [x, y] -> return (x, y)
      _ -> mzero

-- left-associative by default
isAssoBinary :: (Symbolic a, MonadPlus m) => Symbol -> a -> m (a, a)
isAssoBinary s a =
   case isSymbol s a of
      Just [x, y] -> return (x, y)
      Just (x:xs) | length xs > 1 -> return (x, function s xs)
      _ -> mzero