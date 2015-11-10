{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- Copyright 2015, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- A simple data type for term rewriting
--
-----------------------------------------------------------------------------

module Ideas.Common.Rewriting.Term
   ( -- * Symbols
     Symbol, newSymbol
   , isAssociative, makeAssociative
     -- * Terms
   , Term(..), IsTerm(..), termView
   , fromTermM, fromTermWith
     -- * Functions and symbols
   , WithFunctions(..), isSymbol, isFunction
   , unary, binary, ternary, isUnary, isBinary
     -- * Variables
   , WithVars(..), isVariable
   , vars, varSet, hasVar, withoutVar
   , hasSomeVar, hasNoVar, variableView
     -- * Meta variables
   , WithMetaVars(..), isMetaVar
   , metaVars, metaVarSet, hasMetaVar, nextMetaVar
   ) where

import Control.Monad
import Data.Data
import Data.Function
import Data.Maybe
import Ideas.Common.Id
import Ideas.Common.Utils (ShowString(..))
import Ideas.Common.Utils.QuickCheck
import Ideas.Common.Utils.Uniplate
import Ideas.Common.View
import qualified Data.IntSet as IS
import qualified Data.Set as S

-----------------------------------------------------------
-- Symbols

data Symbol = S { isAssociative :: Bool, symbolId :: Id }

instance Eq Symbol where
   (==) = (==) `on` getId -- without associativity property

instance Ord Symbol where
   compare = compareId    -- without associativity property

instance Show Symbol where
   show = showId

instance Read Symbol where
   readsPrec n = map f . readsPrec n
    where
      f :: (Id, String) -> (Symbol, String)
      f (a, s) = (newSymbol a, s)

instance HasId Symbol where
   getId = symbolId
   changeId f (S b a) = S b (f a)

newSymbol :: IsId a => a -> Symbol
newSymbol = S False . newId

makeAssociative :: Symbol -> Symbol
makeAssociative (S _ a) = S True a

-----------------------------------------------------------
-- * Data type for terms

data Term = TVar   String
          | TCon   Symbol [Term]
          | TList  [Term]
          | TNum   Integer
          | TFloat Double
          | TMeta  Int
 deriving (Show, Read, Eq, Ord, Typeable)

instance Uniplate Term where
   uniplate (TCon x xs)   = plate (function x) ||* xs
   uniplate (TList xs)    = plate TList ||* xs
   uniplate term          = plate term

-----------------------------------------------------------
-- * Type class for conversion to/from terms

class IsTerm a where
   toTerm   :: a -> Term
   fromTerm :: MonadPlus m => Term -> m a

termView :: IsTerm a => View Term a
termView = makeView fromTerm toTerm

instance IsTerm Term where
   toTerm   = id
   fromTerm = return

instance IsTerm ShowString where
   toTerm = TVar . fromShowString
   fromTerm (TVar s) = return (ShowString s)
   fromTerm _        = fail "fromTerm"

instance (IsTerm a, IsTerm b) => IsTerm (a, b) where
   toTerm (a, b) = TList [toTerm a, toTerm b]
   fromTerm (TList [a, b]) = liftM2 (,) (fromTerm a) (fromTerm b)
   fromTerm _              = fail "fromTerm"

instance (IsTerm a, IsTerm b) => IsTerm (Either a b) where
   toTerm = either toTerm toTerm
   fromTerm expr =
      liftM Left  (fromTerm expr) `mplus`
      liftM Right (fromTerm expr)

instance IsTerm Int where
   toTerm = TNum . fromIntegral
   fromTerm = liftM fromInteger . fromTerm

instance IsTerm Integer where
   toTerm = TNum
   fromTerm (TNum a) = return a
   fromTerm _        = fail "fromTerm"

instance IsTerm Double where
   toTerm = TFloat
   fromTerm (TFloat a) = return a
   fromTerm _          = fail "fromTerm"

instance IsTerm Char where
   toTerm c = TVar [c]
   fromTerm (TVar [c]) = return c
   fromTerm _          = fail "fromTerm"

instance IsTerm a => IsTerm [a] where
   toTerm = TList . map toTerm
   fromTerm (TList xs) = mapM fromTerm xs
   fromTerm _ = fail "fromTerm"

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
         _            -> fail "Ideas.Common.Term.getSymbol"

instance WithFunctions Term where
   function = TCon
   getFunction (TCon s xs) = return (s, xs)
   getFunction _           = fail "Ideas.Common.Rewriting.getFunction"

isSymbol :: WithFunctions a => Symbol -> a -> Bool
isSymbol s = maybe False (==s) . getSymbol

isFunction :: (WithFunctions a, Monad m) => Symbol -> a -> m [a]
isFunction s a =
   case getFunction a of
      Just (t, as) | s == t -> return as
      _                     -> fail "Ideas.Common.Term.isFunction"

unary :: WithFunctions a => Symbol -> a -> a
unary s a = function s [a]

binary :: WithFunctions a => Symbol -> a -> a -> a
binary s a b = function s [a, b]

ternary :: WithFunctions a => Symbol -> a -> a -> a -> a
ternary s a b c = function s [a, b, c]

isUnary :: (WithFunctions a, Monad m) => Symbol -> a -> m a
isUnary s a =
   case isFunction s a of
      Just [x] -> return x
      _        -> fail "Ideas.Common.Term.isUnary"

isBinary :: (WithFunctions a, Monad m) => Symbol -> a -> m (a, a)
isBinary s a =
   case isFunction s a of
      Just [x, y] -> return (x, y)
      _           -> fail "Ideas.Common.Term.isBinary"

-----------------------------------------------------------
-- * Variables

class WithVars a where
   variable     :: String -> a
   getVariable  :: Monad m => a -> m String

instance WithVars Term where
   variable = TVar
   getVariable (TVar s) = return s
   getVariable _        = fail "Ideas.Common.Rewriting.getVariable"

isVariable :: WithVars a => a -> Bool
isVariable = isJust . getVariable

vars :: (Uniplate a, WithVars a) => a -> [String]
vars = concatMap getVariable . universe

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

variableView :: WithVars a => View a String
variableView = makeView getVariable variable

-----------------------------------------------------------
-- * Meta variables

class WithMetaVars a where
   metaVar    :: Int -> a
   getMetaVar :: Monad m => a -> m Int

instance WithMetaVars Term where
   metaVar = TMeta
   getMetaVar (TMeta i) = return i
   getMetaVar _         = fail "Ideas.Common.Rewriting.getMetaVar"

isMetaVar :: WithMetaVars a => a -> Bool
isMetaVar = isJust . getMetaVar

metaVars :: (Uniplate a, WithMetaVars a) => a -> [Int]
metaVars = concatMap getMetaVar . universe

metaVarSet :: (Uniplate a, WithMetaVars a) => a -> IS.IntSet
metaVarSet = IS.fromList . metaVars

hasMetaVar :: (Uniplate a, WithMetaVars a) => Int -> a -> Bool
hasMetaVar i = (i `elem`) . metaVars

nextMetaVar :: (Uniplate a, WithMetaVars a) => a -> Int
nextMetaVar a
   | null is   = 0
   | otherwise = maximum is + 1
 where
   is = metaVars a

-----------------------------------------------------------
-- * Arbitrary term generator

instance Arbitrary Term where
   arbitrary = generators
      [ constGens $ map TVar ["x", "y", "z"]
      , arbGen TNum, arbGen TFloat, arbGen TMeta
      , constGens $ map (symbol . newSymbol) ["a", "b"]
      , unaryGens $ map (unary . newSymbol) ["h", "k"]
      , binaryGens $ map (binary . newSymbol) ["f", "g"]
      ]