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
-- A simple data type for term rewriting
--
-----------------------------------------------------------------------------

module Ideas.Common.Rewriting.Term.Data
   ( -- * Symbols
     Symbol, newSymbol
   , isAssociative, makeAssociative
     -- * Terms
   , Term(..)
   ) where

import Data.Function
import Ideas.Common.Id
import Ideas.Utils.QuickCheck hiding (function)
import Ideas.Utils.Uniplate

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
 deriving (Show, Read, Eq, Ord)

instance Uniplate Term where
   uniplate (TCon x xs)   = plate (TCon x) ||* xs
   uniplate (TList xs)    = plate TList ||* xs
   uniplate term          = plate term

-----------------------------------------------------------
-- * Arbitrary term generator

instance Arbitrary Term where
   arbitrary = generators
      [ constGens $ map TVar ["x", "y", "z"]
      , arbGen TNum, arbGen TFloat, arbGen TMeta
      , constGens $ map (\s -> TCon (newSymbol s) []) ["a", "b"]
      , unaryGens $ map (\s x -> TCon (newSymbol s) [x]) ["h", "k"]
      , binaryGens $ map (\s x y -> TCon (newSymbol s) [x, y]) ["f", "g"]
      ]