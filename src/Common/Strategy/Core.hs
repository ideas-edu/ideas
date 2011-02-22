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
   , coreMany, coreRepeat, coreOrElse, coreFix, coreParallel
   , CoreEnv, emptyCoreEnv, insertCoreEnv, lookupCoreEnv, substCoreEnv
   , substCoreVar
   ) where

import Common.Transformation
import Common.Uniplate
import Control.Applicative 
import Data.Maybe
import Data.Foldable (Foldable, foldMap)
import qualified Data.Traversable as T
import qualified Data.IntMap as IM

-----------------------------------------------------------------
-- Strategy (internal) data structure, containing a selection
-- of combinators

infixr 2 :|||:
infixr 3 :|:, :|>:
infixr 5 :*:

-- | Core expression, with rules 
type Core l a = GCore l (Rule a)

-- | A generalized Core expression, not restricted to rules. This makes GCore
-- a (traversable and foldable) functor.
data GCore l a
   = GCore l a :*:   GCore l a
   | GCore l a :|:   GCore l a
   | GCore l a :|>:  GCore l a
   | GCore l a :|||: GCore l a -- interleave 
   | GCore l a :||-: GCore l a -- interleave-first-from-left
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
            a :|||: b -> (:|||:) <$> rec a <*> rec b
            a :||-: b -> (:||-:) <$> rec a <*> rec b
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
         a :|||: b -> ([a,b], \[x,y] -> x :|||: y)
         a :||-: b -> ([a,b], \[x,y] -> x :||-: y)
         Many a    -> ([a],   \[x]   -> Many x)
         Repeat a  -> ([a],   \[x]   -> Repeat x)
         Label l a -> ([a],   \[x]   -> Label l x)
         Atomic a  -> ([a],   \[x]   -> Atomic x)
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

substCoreVar :: Int -> Core l a -> Core l a -> Core l a
substCoreVar i a = substCoreEnv (insertCoreEnv i a emptyCoreEnv)

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

-----------------------------------------------------------------
-- Parallel parsing: a first approach

coreParallel :: Core l a -> Core l a -> Core l a
coreParallel Succeed b = b
coreParallel a Succeed = a
coreParallel a b = alts $
   [ x .*. (y :|||: b) | (x, y) <- splitAtom a ] ++
   [ x .*. (a :|||: y) | (x, y) <- splitAtom b ]
 where
   alts [] = Fail
   alts xs = foldr1 (:|:) xs

(.*.) :: Core l a -> Core l a -> Core l a
Fail    .*. _       = Fail
_       .*. Fail    = Fail
Succeed .*. a       = a
a       .*. Succeed = a
a       .*. b       = a :*: b

splitAtom :: Core l a -> [(Core l a, Core l a)]
splitAtom core =
   case core of
      a :*: b   -> [ (x, y .*. b) | (x, y) <- splitAtom a ]
      a :|: b   -> splitAtom a ++ splitAtom b
      a :|||: b -> [ (x, y :|||: b) | (x, y) <- splitAtom a ] ++
                   [ (x, a :|||: y) | (x, y) <- splitAtom b ]
      _ :||-: _  -> undefined
      Rec i a   -> splitAtom (substCoreVar i core a)
      Var _     -> error "Free core var in splitAtom"
      a :|>: b  -> splitAtom (coreOrElse a b)
      Many a    -> splitAtom (coreMany a)
      Repeat a  -> splitAtom (coreRepeat a)
      Fail      -> []
      Atomic a  -> [(a, Succeed)]
      Succeed   -> [(core, Succeed)]
      Rule _    -> [(core, Succeed)]
      Not _     -> [(core, Succeed)]
      Label _ _ -> [(core, Succeed)]

-----------------------------------------------------------------
-- Utility functions

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

mapLabel :: (l -> l) -> Core l a -> Core l a
mapLabel f = run
 where
   run (Label l a) = Label (f l) (run a)
   run core        = descend run core

noLabels :: Core l a -> Core l a
noLabels (Label _ a) = noLabels a
noLabels core        = descend noLabels core