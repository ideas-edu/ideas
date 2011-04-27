{-# LANGUAGE DeriveDataTypeable #-}
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
   , Symbol, newSymbol
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
import Test.QuickCheck

-----------------------------------------------------------
-- * Data type for terms

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

newtype Symbol = S Id
   deriving (Eq, Ord)

instance Show Symbol where
   show = showId

instance HasId Symbol where
   getId (S a) = a
   changeId f (S a) = S (f a)

newSymbol :: IsId a => a -> Symbol
newSymbol = S . newId

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

instance IsTerm Int where
   toTerm = Num . fromIntegral
   fromTerm = liftM fromInteger . fromTerm

instance IsTerm Integer where
   toTerm = Num
   fromTerm (Num a) = return a
   fromTerm _       = fail "fromTerm"

instance IsTerm Double where
   toTerm = Float
   fromTerm (Float a) = return a
   fromTerm _         = fail "fromTerm"

fromTermM :: (Monad m, IsTerm a) => Term -> m a
fromTermM = maybe (fail "fromTermM") return . fromTerm

fromTermWith :: (Monad m, IsTerm a) => (Symbol -> [a] -> m a) -> Term -> m a
fromTermWith f a = do
   (s, xs) <- getFunction a
   ys      <- mapM fromTermM xs
   f s ys

-----------------------------------------------------------
-- * Functions and symbols

class WithFunctions a where
   -- constructing
   symbol   :: Symbol -> a
   function :: Symbol -> [a] -> a
   -- matching
   getSymbol   :: Monad m => a -> m Symbol
   getFunction :: Monad m => a -> m (Symbol, [a])
   -- default definition
   symbol s = function s []
   getSymbol a = 
      case getFunction a of
         Just (t, []) -> return t
         _            -> fail "Common.Term.getSymbol"
         
instance WithFunctions Term where
   function = makeTerm . Con
   getFunction a = 
      case getSpine a of
         (Con s, xs) -> return (s, xs)
         _           -> fail "Common.Rewriting.getFunction" 
   
isSymbol :: WithFunctions a => Symbol -> a -> Bool
isSymbol s = maybe False (==s) . getSymbol

isFunction :: (WithFunctions a, Monad m) => Symbol -> a -> m [a]
isFunction s a =
   case getFunction a of
      Just (t, as) | s == t -> return as
      _                     -> fail "Common.Term.isFunction"

unary :: WithFunctions a => Symbol -> a -> a
unary s a = function s [a]

binary :: WithFunctions a => Symbol -> a -> a -> a
binary s a b = function s [a, b]

isUnary :: (WithFunctions a, Monad m) => Symbol -> a -> m a
isUnary s a = 
   case isFunction s a of
      Just [x] -> return x
      _        -> fail "Common.Term.isUnary"

isBinary :: (WithFunctions a, Monad m) => Symbol -> a -> m (a, a)
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

-----------------------------------------------------------
-- * Arbitrary term generator

instance Arbitrary Term where
   arbitrary = sized arbTerm
    where
      arbTerm 0 = oneof
         [ oneof $ map (return . Var) ["x", "y", "z"]
         , oneof [liftM Num arbitrary, liftM Float arbitrary]
         , liftM Meta arbitrary
         , oneof $ map (return . Con . newSymbol) ["a", "b"]
         ]
      arbTerm n = oneof
         [ arbTerm 0
         , oneof [ liftM2 (binary (newSymbol s)) rec rec | s <- ["f", "g"] ]
         , oneof [ liftM (unary (newSymbol s)) rec | s <- ["h", "k"] ]
         ]
       where
         rec = arbTerm (n `div` 2)