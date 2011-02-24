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
-- The core strategy combinators. This module defines the interal data
-- structure of a strategy, and some utility functions that operate 
-- directly on it.
--
-----------------------------------------------------------------------------
module Common.Strategy.Core 
   ( GCore(..), Core
   , mapLabel, noLabels
   , coreMany, coreRepeat, coreOrElse, coreFix
   , substCoreVar
   ) where

import Common.Transformation
import Common.Uniplate
import Control.Applicative 
import Data.Foldable (Foldable, foldMap)
import qualified Data.Traversable as T

-----------------------------------------------------------------
-- Strategy (internal) data structure, containing a selection
-- of combinators

infixr 2 :%:, :!%:
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
   | GCore l a :!%: GCore l a -- interleave-first-from-left
   | Many    (GCore l a)
   | Repeat  (GCore l a)
   | Not     (GCore l a)
   | Label l (GCore l a)
   | Atomic  (GCore l a)
   | Succeed
   | Fail
   | Rule a -- ^ Generalized constructor (not restricted to rules)
   | Var Int
   | Rec Int (GCore l a)
 deriving Show

-----------------------------------------------------------------
-- Useful instances

instance Functor (GCore l) where
   fmap = T.fmapDefault

instance Foldable (GCore l) where
   foldMap = T.foldMapDefault 
   
instance T.Traversable (GCore l) where
   traverse f = rec
    where
      rec core =
         case core of
            a :*: b   -> (:*:)   <$> rec a <*> rec b
            a :|: b   -> (:|:)   <$> rec a <*> rec b
            a :|>: b  -> (:|>:)  <$> rec a <*> rec b
            a :%: b   -> (:%:)   <$> rec a <*> rec b
            a :!%: b  -> (:!%:)  <$> rec a <*> rec b
            Many a    -> Many    <$> rec a
            Repeat a  -> Repeat  <$> rec a
            Not a     -> Not     <$> rec a
            Label l a -> Label l <$> rec a
            Atomic a  -> Atomic  <$> rec a
            Rec n a   -> Rec n   <$> rec a
            Rule a    -> Rule    <$> f a
            Var n     -> pure (Var n)
            Succeed   -> pure Succeed
            Fail      -> pure Fail

instance Uniplate (GCore l a) where
   uniplate core =
      case core of
         a :*: b   -> ([a,b], \[x,y] -> x :*: y)
         a :|: b   -> ([a,b], \[x,y] -> x :|: y)
         a :|>: b  -> ([a,b], \[x,y] -> x :|>: y)
         a :%: b   -> ([a,b], \[x,y] -> x :%: y)
         a :!%: b  -> ([a,b], \[x,y] -> x :!%: y)
         Many a    -> ([a],   \[x]   -> Many x)
         Repeat a  -> ([a],   \[x]   -> Repeat x)
         Label l a -> ([a],   \[x]   -> Label l x)
         Atomic a  -> ([a],   \[x]   -> Atomic x)
         Rec n a   -> ([a],   \[x]   -> Rec n x)
         Not a     -> ([a],   \[x]   -> Not x)
         _         -> ([],    \_     -> core)

-----------------------------------------------------------------
-- Definitions

coreMany :: GCore l a -> GCore l a
coreMany a = Rec n (Succeed :|: (a :*: Var n))
 where n = nextVar a

coreRepeat :: GCore l a -> GCore l a
coreRepeat a = Many a :*: Not a

coreOrElse :: GCore l a -> GCore l a -> GCore l a
coreOrElse a b = a :|: (Not a :*: b)

coreFix :: (GCore l a -> GCore l a) -> GCore l a
coreFix f = -- disadvantage: function f is applied twice
   let i = nextVar (f (Var (-1)))
   in Rec i (f (Var i))

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

mapLabel :: (l -> l) -> GCore l a -> GCore l a
mapLabel f = run
 where
   run (Label l a) = Label (f l) (run a)
   run core        = descend run core

noLabels :: GCore l a -> GCore l a
noLabels (Label _ a) = noLabels a
noLabels core        = descend noLabels core