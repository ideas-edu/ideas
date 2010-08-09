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
   , mapRule, coreVars, noLabels, mapCore, mapCoreM
   , mapLabel, coreFix
   , coreMany, coreRepeat
   , CoreEnv, emptyCoreEnv, insertCoreEnv, lookupCoreEnv
   ) where

import Common.Transformation
import Common.Uniplate
import Control.Monad.Identity
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

newtype CoreEnv l a = CE (IM.IntMap (CoreEnv l a, Core l a)) 

emptyCoreEnv :: CoreEnv l a
emptyCoreEnv = CE IM.empty
  
insertCoreEnv :: Int -> Core l a -> CoreEnv l a -> CoreEnv l a
insertCoreEnv n a env@(CE m) = CE (IM.insert n (env, a) m)
  
lookupCoreEnv :: Int -> CoreEnv l a -> Maybe (CoreEnv l a, Core l a)
lookupCoreEnv n (CE m) = do
   (e, a) <- IM.lookup n m
   return (e, Rec n a)



coreMany :: Core l a -> Core l a
coreMany p = Rec n (Succeed :|: (p :*: Var n))
 where n = nextVar p

coreRepeat :: Core l a -> Core l a
coreRepeat p = Many p :*: Not p

coreFix :: (Core l a -> Core l a) -> Core l a
coreFix f = -- disadvantage: function f is applied twice
   let i = nextVar (f (Var (-1)))
   in Rec i (f (Var i))

nextVar :: Core l a -> Int
nextVar p
   | null xs   = 0
   | otherwise = maximum xs + 1
 where xs = coreVars p

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
mapCore f g = 
   let fm l = return . f l . runIdentity
       gm   = return . g
   in runIdentity . mapCoreM fm gm

-- The most primitive function that applies functions to the label and 
-- rule alternatives. Monadic version.
mapCoreM :: Monad m => (k -> m (Core l b) -> m (Core l b)) 
                    -> (Rule a -> m (Core l b)) 
                    -> Core k a -> m (Core l b)
mapCoreM f g = rec 
 where 
   rec core =
      case core of
         a :*: b   -> liftM2 (:*:)  (rec a) (rec b)
         a :|: b   -> liftM2 (:|:)  (rec a) (rec b)
         a :|>: b  -> liftM2 (:|>:) (rec a) (rec b)
         Many a    -> liftM Many   (rec a)
         Repeat a  -> liftM Repeat (rec a)
         Succeed   -> return Succeed
         Fail      -> return Fail
         Label l a -> f l (rec a)
         Rule r    -> g r
         Var n     -> return (Var n)
         Rec n a   -> liftM (Rec n) (rec a)
         Not a     -> liftM Not (rec a) 
      
coreVars :: Core l a -> [Int]
coreVars core = 
   case core of
      Var n   -> [n]
      Rec n a -> n : coreVars a
      _       -> concatMap coreVars (children core)