-----------------------------------------------------------------------------
-- Copyright 2019, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Generic terms
--
-----------------------------------------------------------------------------

module Ideas.Common.Rewriting.Term.Class
   ( -- * Terms
     IsTerm(..), termView
   , nothingSymbol, trueSymbol, falseSymbol
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

import Data.Maybe
import Ideas.Common.Rewriting.Term.Data
import Ideas.Common.Rewriting.Term.Decoder
import Ideas.Common.Id
import Ideas.Common.View
import Ideas.Utils.Decoding
import Ideas.Utils.Prelude (ShowString(..))
import Ideas.Utils.Uniplate
import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.Set as S

-----------------------------------------------------------
-- * Type class for conversion to/from terms

class IsTerm a where
   toTerm          :: a -> Term
   toTermList      :: [a] -> Term
   fromTerm        :: Term -> Maybe a
   termDecoder     :: TermDecoder a
   termListDecoder :: TermDecoder [a]

   {-# MINIMAL toTerm, termDecoder  #-}

   -- default implementation
   toTermList = TList . map toTerm
   fromTerm t = either (const Nothing) Just (evalDecoder termDecoder () [t])
   termListDecoder = tListOf termDecoder

termView :: IsTerm a => View Term a
termView = makeView fromTerm toTerm

instance IsTerm Term where
   toTerm = id
   termDecoder = tFirst $ maybe (errorStr "not a term") return

instance IsTerm ShowString where
   toTerm = TVar . fromShowString
   termDecoder = ShowString <$> tVar

instance (IsTerm a, IsTerm b) => IsTerm (a, b) where
   toTerm (a, b) = TList [toTerm a, toTerm b]
   termDecoder = tList2 (,) termDecoder termDecoder

instance (IsTerm a, IsTerm b, IsTerm c) => IsTerm (a, b, c) where
   toTerm (a, b, c) = TList [toTerm a, toTerm b, toTerm c]
   termDecoder = tList3 (,,) termDecoder termDecoder termDecoder

instance (IsTerm a, IsTerm b) => IsTerm (Either a b) where
   toTerm = either toTerm toTerm
   termDecoder = Left <$> termDecoder <|> Right <$> termDecoder

instance IsTerm Int where
   toTerm = TNum . fromIntegral
   termDecoder = fromInteger <$> tInteger

instance IsTerm Integer where
   toTerm = TNum
   termDecoder = tInteger

instance IsTerm Double where
   toTerm = TFloat
   termDecoder = tDouble

instance IsTerm Float where
   toTerm = TFloat . realToFrac
   termDecoder = realToFrac <$> tDouble

instance IsTerm Char where
   toTerm c = TVar [c]
   toTermList = TVar
   termDecoder     = tChar
   termListDecoder = tVar

instance IsTerm Bool where
   toTerm True  = symbol trueSymbol
   toTerm False = symbol falseSymbol
   termDecoder  = True <$ tCon0 trueSymbol <|> False <$ tCon0 falseSymbol

instance IsTerm Id where
   toTerm = toTerm . show
   termDecoder = newId <$> tVar

instance IsTerm a => IsTerm [a] where
   toTerm = toTermList
   termDecoder = termListDecoder

instance (IsTerm a, Ord a) => IsTerm (S.Set a) where
   toTerm   = toTerm . S.toList
   termDecoder = S.fromList <$> termDecoder

instance (IsTerm a, IsTerm b, Ord a) => IsTerm (M.Map a b) where
   toTerm = toTerm . M.toList
   termDecoder = M.fromList <$> termDecoder

trueSymbol, falseSymbol, nothingSymbol :: Symbol
trueSymbol    = newSymbol "true"
falseSymbol   = newSymbol "false"
nothingSymbol = newSymbol "Nothing"

instance IsTerm a => IsTerm (Maybe a) where
   toTerm = maybe (symbol nothingSymbol) toTerm
   termDecoder = Just <$> termDecoder <|> Nothing <$ tCon0 nothingSymbol

-----------------------------------------------------------
-- * Functions and symbols

class WithFunctions a where
   -- constructing
   symbol   :: Symbol -> a
   function :: Symbol -> [a] -> a
   -- matching
   getSymbol   :: a -> Maybe Symbol
   getFunction :: a -> Maybe (Symbol, [a])
   -- default definition
   symbol s = function s []
   getSymbol a = fst <$> getFunction a

instance WithFunctions Term where
   function = TCon
   getFunction (TCon s xs) = Just (s, xs)
   getFunction _ = Nothing

isSymbol :: WithFunctions a => Symbol -> a -> Bool
isSymbol s = (== Just s) . getSymbol

isFunction :: WithFunctions a => Symbol -> a -> Maybe [a]
isFunction s a =
   case getFunction a of
      Just (t, as) | s == t -> Just as
      _ -> Nothing

unary :: WithFunctions a => Symbol -> a -> a
unary s a = function s [a]

binary :: WithFunctions a => Symbol -> a -> a -> a
binary s a b = function s [a, b]

ternary :: WithFunctions a => Symbol -> a -> a -> a -> a
ternary s a b c = function s [a, b, c]

isUnary :: WithFunctions a => Symbol -> a -> Maybe a
isUnary s a =
   case isFunction s a of
      Just [x] -> Just x
      _ -> Nothing

isBinary :: WithFunctions a => Symbol -> a -> Maybe (a, a)
isBinary s a =
   case isFunction s a of
      Just [x, y] -> Just (x, y)
      _ -> Nothing

-----------------------------------------------------------
-- * Variables

class WithVars a where
   variable     :: String -> a
   getVariable  :: a -> Maybe String

instance WithVars Term where
   variable = TVar
   getVariable (TVar s) = Just s
   getVariable _ = Nothing

isVariable :: WithVars a => a -> Bool
isVariable = isJust . getVariable

vars :: (Uniplate a, WithVars a) => a -> [String]
vars = mapMaybe getVariable . universe

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
   getMetaVar :: a -> Maybe Int

instance WithMetaVars Term where
   metaVar = TMeta
   getMetaVar (TMeta i) = Just i
   getMetaVar _ = Nothing

isMetaVar :: WithMetaVars a => a -> Bool
isMetaVar = isJust . getMetaVar

metaVars :: (Uniplate a, WithMetaVars a) => a -> [Int]
metaVars = mapMaybe getMetaVar . universe

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