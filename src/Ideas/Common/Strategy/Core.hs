-----------------------------------------------------------------------------
-- Copyright 2013, Open Universiteit Nederland. This file is distributed
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
module Ideas.Common.Strategy.Core
   ( GCore(..), Core
   , coreFix, coreSubstAll
   , noLabels, substCoreVar
   ) where

import Ideas.Common.Classes
import Ideas.Common.Rule
import Ideas.Common.Utils.QuickCheck
import Ideas.Common.Utils.Uniplate
import Ideas.Common.Strategy.Sequential

-----------------------------------------------------------------
-- Strategy (internal) data structure, containing a selection
-- of combinators

infixr 2 :%:, :@:
infixr 3 :|:, :|>:
infixr 5 :*:

-- | Core expression, with rules
type Core l a = GCore l (Rule a)

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
   | Rec Int (GCore l a)
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
         Rec n a   -> plate Rec    |- n |* a
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
            Rec n a   -> Rec n  (rec a)
            Label l a -> Label (f l) (rec a)
            Rule a    -> Rule (g a)
            Var n     -> Var n
            Succeed   -> Succeed
            Fail      -> Fail

{-
instance T.Traversable (GCore l) where
   traverse f core =
      case core of
         a :*: b   -> (:*:)   <$> T.traverse f a <*> T.traverse f b
         a :|: b   -> (:|:)   <$> T.traverse f a <*> T.traverse f b
         a :|>: b  -> (:|>:)  <$> T.traverse f a <*> T.traverse f b
         a :%: b   -> (:%:)   <$> T.traverse f a <*> T.traverse f b
         Label l a -> Label l <$> T.traverse f a
         Atomic a  -> Atomic  <$> T.traverse f a
         Rec n a   -> Rec n   <$> T.traverse f a
         Rule r    -> Rule    <$> f r
         Succeed   -> pure Succeed
         Fail      -> pure Fail
         Var n     -> pure $ Var n -}

--instance F.Foldable (GCore l) where
   --foldMap = T.foldMapDefault

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
   in Rec i (f (Var i))

coreSubstAll :: GCore l a -> GCore l a
coreSubstAll = rec []
 where
   rec xs (Var i)   = maybe (error "coreInf") id (lookup i xs)
   rec xs (Rec i a) = let this = rec ((i, this) : xs) a
                      in this
   rec xs core      = descend (rec xs) core 

-----------------------------------------------------------------
-- Utility functions

substCoreVar :: Int -> GCore l a -> GCore l a -> GCore l a
substCoreVar i a core =
   case core of
      Var j   | i==j -> a
      Rec j _ | i==j -> core
      _              -> descend (substCoreVar i a) core

nextVar :: GCore l a -> Int
nextVar p
   | null xs   = 0
   | otherwise = maximum xs + 1
 where xs = coreVars p

coreVars :: GCore l a -> [Int]
coreVars core =
   case core of
      Var n   -> [n]
      Rec n a -> n : coreVars a
      _       -> concatMap coreVars (children core)

noLabels :: GCore l a -> GCore l a
noLabels (Label _ a) = noLabels a
noLabels core        = descend noLabels core