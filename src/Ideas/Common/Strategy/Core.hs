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
   , coreFix, coreSubstAll, substCoreVar
   ) where

import Data.Maybe
import Ideas.Common.Classes
import Ideas.Common.Id
import Ideas.Common.Rule
import Ideas.Common.Strategy.Choice
import Ideas.Common.Strategy.Sequence
import Ideas.Common.Utils.QuickCheck
import Ideas.Common.Utils.Uniplate

-----------------------------------------------------------------
-- Strategy (internal) data structure, containing a selection
-- of combinators

infixr 2 :%:, :@:
infixr 3 :|:, :|>:
infixr 5 :*:

-- | Core expression, with rules
type Core a = GCore (Rule a)

-- | An environment with generalized Core expressions
type CoreEnv a = [(Int, GCore a)]

-- | A generalized Core expression, not restricted to rules. This makes GCore
-- a (traversable and foldable) functor.
data GCore a
   = GCore a :*:  GCore a
   | GCore a :|:  GCore a
   | GCore a :|>: GCore a
   | GCore a :%:  GCore a -- interleave
   | GCore a :@:  GCore a -- alternate
   | Label Id (GCore a)
   | Atomic   (GCore a)
   | Remove   (GCore a) -- config: replaced by fail
   | Collapse (GCore a) -- config: execute labeled sub-strategy as 1 step
   | Hide     (GCore a) -- config: make all steps invisible/minor
   | Succeed
   | Fail
   | Rule a -- ^ Generalized constructor (not restricted to rules)
   | Var Int
   | Let (CoreEnv a) (GCore a)
 deriving Show

instance Choice GCore where
   empty  = Fail
   single = Rule
   (<|>)  = (:|:)
   (|>)   = (:|>:)
   
instance Sequence GCore where
   done  = Succeed
   (~>)  = (:*:) . Rule
   (<*>) = (:*:)

-----------------------------------------------------------------
-- Useful instances

instance Functor GCore where
   fmap f = rec
    where
      rec core =
         case core of
            a :*: b    -> rec a :*:  rec b
            a :|: b    -> rec a :|:  rec b
            a :|>: b   -> rec a :|>: rec b
            a :%: b    -> rec a :%:  rec b
            a :@: b    -> rec a :@:  rec b
            Atomic a   -> Atomic   (rec a)
            Remove a   -> Remove   (rec a)
            Collapse a -> Collapse (rec a)
            Hide a     -> Hide     (rec a)
            Let ds a   -> Let (map (mapSecond rec) ds) (rec a)
            Label l a  -> Label l (rec a)
            Rule a     -> Rule (f a)
            Var n      -> Var n
            Succeed    -> Succeed
            Fail       -> Fail
            
instance Uniplate (GCore a) where
   uniplate core =
      case core of
         a :*: b    -> plate (:*:)  |* a |* b
         a :|: b    -> plate (:|:)  |* a |* b
         a :|>: b   -> plate (:|>:) |* a |* b
         a :%: b    -> plate (:%:)  |* a |* b
         a :@: b    -> plate (:@:)  |* a |* b
         Label l a  -> plate Label  |- l |* a
         Atomic a   -> plate Atomic   |* a
         Remove a   -> plate Remove   |* a
         Collapse a -> plate Collapse |* a
         Hide a     -> plate Hide     |* a
         Let ds a   -> let (ns, bs) = unzip ds
                           make     = Let . zip ns
                       in plate make ||* bs |* a
         _          -> plate core

instance Arbitrary a => Arbitrary (GCore a) where
   arbitrary = generators
      [ constGens [Succeed, Fail]
      , unaryGen Atomic, arbGen Rule, unaryArbGen Label
      , binaryGens [(:*:), (:|:), (:%:)]
      ]

-----------------------------------------------------------------
-- Definitions

coreFix :: (GCore a -> GCore a) -> GCore a
coreFix f = -- disadvantage: function f is applied twice
   let i = nextVar (f (Var (-1)))
   in coreRec i (f (Var i))

coreRec :: Int -> GCore a -> GCore a
coreRec n a = Let [(n, a)] (Var n)

coreSubstAll :: GCore a -> GCore a
coreSubstAll = rec []
 where
   rec xs (Var i)    = fromMaybe (error "coreInf") (lookup i xs)
   rec xs (Let ds a) = let this = [ (n, rec this b) | (n, b) <- ds ] ++ xs
                       in rec this a
   rec xs core       = descend (rec xs) core

-----------------------------------------------------------------
-- Utility functions

substCoreVar :: Int -> GCore a -> GCore a -> GCore a
substCoreVar i a core =
   case core of
      Var j    | i==j -> a
      Let ds _ | i `elem` map fst ds -> core
      _               -> descend (substCoreVar i a) core

nextVar :: GCore a -> Int
nextVar p
   | null xs   = 0
   | otherwise = maximum xs + 1
 where xs = coreVars p

coreVars :: GCore a -> [Int]
coreVars core =
   case core of
      Var n    -> [n]
      Let ds a -> let (ns, bs) = unzip ds
                  in ns ++ concatMap coreVars (bs ++ [a])
      _        -> concatMap coreVars (children core)