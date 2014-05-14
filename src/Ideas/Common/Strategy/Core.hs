-----------------------------------------------------------------------------
-- Copyright 2014, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- The core strategy combinators. This module defines the interal data
-- structure of a strategy, and some utility functions that operate
-- directly on it.
--
-----------------------------------------------------------------------------
--  $Id$

module Ideas.Common.Strategy.Core
   ( GCore(..), Core
   , coreFix, coreSubstAll
   , noLabels, substCoreVar
   ) where

import Data.Maybe
import Ideas.Common.Classes
import Ideas.Common.Rule
import Ideas.Common.Strategy.Sequential
import Ideas.Common.Utils.QuickCheck
import Ideas.Common.Utils.Uniplate

-----------------------------------------------------------------
-- Strategy (internal) data structure, containing a selection
-- of combinators

infixr 2 :%:, :@:
infixr 3 :|:, :|>:
infixr 5 :*:

-- | Core expression, with rules
type Core l a = GCore l (Rule a)

-- | An environment with generalized Core expressions
type CoreEnv l a = [(Int, GCore l a)]

-- | A generalized Core expression, not restricted to rules. This makes GCore
-- a (traversable and foldable) functor.
data GCore l a
   = GCore l a :*:  GCore l a
   | GCore l a :|:  GCore l a
   | GCore l a :|>: GCore l a
   | GCore l a :%:  GCore l a -- interleave
   | GCore l a :@:  GCore l a -- alternate
   | Label l (GCore l a)
   | Atomic  (GCore l a)
   | Succeed
   | Fail
   | Rule a -- ^ Generalized constructor (not restricted to rules)
   | Var Int
   | Let (CoreEnv l a) (GCore l a)
 deriving Show

instance Sequential (GCore l) where
   ok     = Succeed
   stop   = Fail
   single = Rule
   (<|>)  = (:|:)
   (<?>)  = (:|>:)
   (<*>)  = (:*:)

-----------------------------------------------------------------
-- Useful instances

instance Functor (GCore l) where
   fmap = mapSecond

instance Uniplate (GCore l a) where
   uniplate core =
      case core of
         a :*: b   -> plate (:*:)  |* a |* b
         a :|: b   -> plate (:|:)  |* a |* b
         a :|>: b  -> plate (:|>:) |* a |* b
         a :%: b   -> plate (:%:)  |* a |* b
         a :@: b   -> plate (:@:)  |* a |* b
         Label l a -> plate Label  |- l |* a
         Atomic a  -> plate Atomic |* a
         Let ds a  -> let (ns, bs) = unzip ds
                          make     = Let . zip ns
                      in plate make ||* bs |* a
         _         -> plate core

instance BiFunctor GCore where
   biMap f g = rec
    where
      rec core =
         case core of
            a :*: b   -> rec a :*:  rec b
            a :|: b   -> rec a :|:  rec b
            a :|>: b  -> rec a :|>: rec b
            a :%: b   -> rec a :%:  rec b
            a :@: b   -> rec a :@:  rec b
            Atomic a  -> Atomic (rec a)
            Let ds a  -> Let (map (mapSecond rec) ds) (rec a)
            Label l a -> Label (f l) (rec a)
            Rule a    -> Rule (g a)
            Var n     -> Var n
            Succeed   -> Succeed
            Fail      -> Fail

instance (Arbitrary l, Arbitrary a) => Arbitrary (GCore l a) where
   arbitrary = generators
      [ constGens [Succeed, Fail]
      , unaryGen Atomic, arbGen Rule, unaryArbGen Label
      , binaryGens [(:*:), (:|:), (:%:)]
      ]

-----------------------------------------------------------------
-- Definitions

coreFix :: (GCore l a -> GCore l a) -> GCore l a
coreFix f = -- disadvantage: function f is applied twice
   let i = nextVar (f (Var (-1)))
   in coreRec i (f (Var i))

coreRec :: Int -> GCore l a -> GCore l a
coreRec n a = Let [(n, a)] (Var n)

coreSubstAll :: GCore l a -> GCore l a
coreSubstAll = rec []
 where
   rec xs (Var i)    = fromMaybe (error "coreInf") (lookup i xs)
   rec xs (Let ds a) = let this = [ (n, rec this b) | (n, b) <- ds ] ++ xs
                       in rec this a
   rec xs core       = descend (rec xs) core

-----------------------------------------------------------------
-- Utility functions

substCoreVar :: Int -> GCore l a -> GCore l a -> GCore l a
substCoreVar i a core =
   case core of
      Var j    | i==j -> a
      Let ds _ | i `elem` map fst ds -> core
      _               -> descend (substCoreVar i a) core

nextVar :: GCore l a -> Int
nextVar p
   | null xs   = 0
   | otherwise = maximum xs + 1
 where xs = coreVars p

coreVars :: GCore l a -> [Int]
coreVars core =
   case core of
      Var n    -> [n]
      Let ds a -> let (ns, bs) = unzip ds
                  in ns ++ concatMap coreVars (bs ++ [a])
      _        -> concatMap coreVars (children core)

noLabels :: GCore l a -> GCore l a
noLabels (Label _ a) = noLabels a
noLabels core        = descend noLabels core