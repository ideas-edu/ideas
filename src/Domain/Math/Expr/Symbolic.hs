module Domain.Math.Expr.Symbolic where

import Control.Monad

-------------------------------------------------------------
-- Symbol data type

data Symbol = Symbol 
   { symbolName :: String
   , arity      :: Maybe Int
   , extraNames :: [String]
   }

instance Show Symbol where
   show = symbolName

instance Eq Symbol where
   a == b = symbolName a == symbolName b

instance Ord Symbol where
   compare a b = compare (symbolName a) (symbolName b)

makeConst :: String -> Symbol
makeConst s = makeSymbol s 0

makeSymbol :: String -> Int -> Symbol
makeSymbol s n = Symbol s (Just n) []

makeSymbolN :: String -> Symbol
makeSymbolN s = Symbol s Nothing []

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