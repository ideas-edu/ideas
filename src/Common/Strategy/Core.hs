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
   ( Core(..)
   , mapRule, mapLabel, noLabels
   , coreMany, coreRepeat, coreOrElse, coreFix
   , CoreEnv, emptyCoreEnv, insertCoreEnv, lookupCoreEnv, substCoreEnv
   ) where

import Common.Transformation
import Common.Uniplate
import Data.Maybe
import qualified Data.IntMap as IM

-----------------------------------------------------------------
-- Strategy (internal) data structure, containing a selection
-- of combinators

infixr 3 :|:, :|>:
infixr 5 :*:

-- Some rules receive label (but not all)
data Core l a
   = Core l a :*:  Core l a
   | Core l a :|:  Core l a
   | Core l a :|>: Core l a
   | Many   (Core l a)
   | Repeat (Core l a)
   | Not (Core l a)
   | Label l (Core l a)
   | Succeed
   | Fail
   | Rule (Rule a)
   | Var Int
   | Rec Int (Core l a)
 deriving Show

-----------------------------------------------------------------
-- Useful instances

instance Uniplate (Core l a) where
   uniplate core =
      case core of
         a :*: b   -> ([a,b], \[x,y] -> x :*: y)
         a :|: b   -> ([a,b], \[x,y] -> x :|: y)
         a :|>: b  -> ([a,b], \[x,y] -> x :|>: y)
         Many a    -> ([a],   \[x]   -> Many x)
         Repeat a  -> ([a],   \[x]   -> Repeat x)
         Label l a -> ([a],   \[x]   -> Label l x)
         Rec n a   -> ([a],   \[x]   -> Rec n x)
         Not a     -> ([a],   \[x]   -> Not x)
         _         -> ([],    \_     -> core)

-----------------------------------------------------------------
-- Core environment

newtype CoreEnv l a = CE (IM.IntMap (Core l a)) 

emptyCoreEnv :: CoreEnv l a
emptyCoreEnv = CE IM.empty
  
insertCoreEnv :: Int -> Core l a -> CoreEnv l a -> CoreEnv l a
insertCoreEnv n a (CE m) = CE (IM.insert n a m)

deleteCoreEnv :: Int -> CoreEnv l a -> CoreEnv l a
deleteCoreEnv n (CE m) = CE (IM.delete n m)

lookupCoreEnv :: Int -> CoreEnv l a -> Maybe (Core l a)
lookupCoreEnv n (CE m) = IM.lookup n m

substCoreEnv :: CoreEnv l a -> Core l a -> Core l a
substCoreEnv env core = 
   case core of
      Var i   -> fromMaybe core (lookupCoreEnv i env)
      Rec i a -> Rec i (substCoreEnv (deleteCoreEnv i env) a)
      _       -> descend (substCoreEnv env) core

-----------------------------------------------------------------
-- Definitions

coreMany :: Core l a -> Core l a
coreMany a = Rec n (Succeed :|: (a :*: Var n))
 where n = nextVar a

coreRepeat :: Core l a -> Core l a
coreRepeat a = Many a :*: Not a

coreOrElse :: Core l a -> Core l a -> Core l a
coreOrElse a b = a :|: (Not a :*: b)

coreFix :: (Core l a -> Core l a) -> Core l a
coreFix f = -- disadvantage: function f is applied twice
   let i = nextVar (f (Var (-1)))
   in Rec i (f (Var i))

nextVar :: Core l a -> Int
nextVar p
   | null xs   = 0
   | otherwise = maximum xs + 1
 where xs = coreVars p

coreVars :: Core l a -> [Int]
coreVars core = 
   case core of
      Var n   -> [n]
      Rec n a -> n : coreVars a
      _       -> concatMap coreVars (children core)

-----------------------------------------------------------------
-- Utility functions

mapLabel :: (l -> m) -> Core l a -> Core m a
mapLabel f = mapCore (Label . f) Rule

mapRule :: (Rule a -> Rule b) -> Core l a -> Core l b
mapRule f = mapCore Label (Rule . f)

noLabels :: Core l a -> Core m a
noLabels = mapCore (const id) Rule
   
mapCore :: (l -> Core m b -> Core m b) -> (Rule a -> Core m b) 
        -> Core l a -> Core m b
mapCore f g = rec
 where
   rec core =
      case core of
         a :*: b   -> rec a :*:  rec b
         a :|: b   -> rec a :|:  rec b
         a :|>: b  -> rec a :|>: rec b
         Many a    -> Many   (rec a)
         Repeat a  -> Repeat (rec a)
         Succeed   -> Succeed
         Fail      -> Fail
         Label l a -> f l (rec a)
         Rule r    -> g r
         Var n     -> Var n
         Rec n a   -> Rec n (rec a)
         Not a     -> Not (rec a)