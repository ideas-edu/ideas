{-# LANGUAGE RankNTypes #-}
-----------------------------------------------------------------------------
-- Copyright 2015, Open Universiteit Nederland. This file is distributed
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
   ( -- * (Generalized) core strategies
     GCore(..), Core, toList, coreFix, coreSubstAll
     -- * Strategy definitions
   , Def, makeDef, makeDef1, makeDef2, makeDefTrans, associativeDef
   , isAssociative, useDef, applyDef0, applyDef1, applyDef2
   , coreDefs
     -- * Step
   , Step(..), stepRule, stepEnvironment
   ) where

import Data.Foldable (Foldable, foldMap, toList)
import Data.List (intercalate)
import Data.Maybe
import Ideas.Common.Classes
import Ideas.Common.Environment
import Ideas.Common.Id
import Ideas.Common.Rule
import Ideas.Common.Strategy.Choice
import Ideas.Common.Strategy.Sequence
import Ideas.Common.Strategy.Process
import Ideas.Common.Strategy.Derived
import Ideas.Common.Utils.Uniplate
import Prelude hiding (sequence)

-----------------------------------------------------------------
-- Strategy (internal) data structure, containing a selection
-- of combinators

-- | Core expression, with rules
type Core a = GCore (Rule a)

-- | An environment with generalized Core expressions
type CoreEnv a = [(Int, GCore a)]

-- | A generalized Core expression, not restricted to rules. This makes GCore
-- a functor.
data GCore a
   = Label Id (GCore a)
   | Apply Def [GCore a]
   | Sym a
   | Var Int
   | Let (CoreEnv a) (GCore a)

applyDef0 :: Def -> GCore a
applyDef0 def = Apply def []

applyDef1 :: Def -> GCore a -> GCore a
applyDef1 def x = Apply def [x]

applyDef2 :: Def -> GCore a -> GCore a -> GCore a
applyDef2 def x y = Apply def [x, y]

data Def = DefPrim Id Bool (forall a . [Builder (Step a)] -> Builder (Step a))
         | DefCore Id (forall a . [Core a] -> Core a)

makeDef :: IsId n => n -> (forall a . [Builder (Step a)] -> Builder (Step a)) -> Def
makeDef n = DefPrim (newId n) False

makeDef1 :: IsId n => n -> (forall a . Builder (Step a) -> Builder (Step a)) -> Def
makeDef1 n f = makeDef n $ \xs -> 
   case xs of
      [a] -> f a
      _   -> empty

makeDef2 :: IsId n => n -> (forall a . Builder (Step a) -> Builder (Step a) -> Builder (Step a)) -> Def
makeDef2 n f = makeDef n $ \xs -> 
   case xs of
      [a, b] -> f a b
      _      -> empty

associativeDef :: IsId n => n -> (forall a . [Builder (Step a)] -> Builder (Step a)) -> Def
associativeDef n = DefPrim (newId n) True

isAssociative :: Def -> Bool
isAssociative (DefPrim _ b _) = b
isAssociative (DefCore _ _)   = False

makeDefTrans :: IsId n => n -> (forall a . Core a -> Core a) -> Def
makeDefTrans n f = DefCore (newId n) $ \xs -> 
   case xs of
      [a] -> f a
      _   -> empty

useDef :: (Core a -> Builder (Step a)) -> Def -> [Core a] -> Builder (Step a)
useDef rec (DefPrim _ _ f) = f . map rec
useDef rec (DefCore _ f)   = rec . f

instance Eq Def where
   x == y = compareId x y == EQ

instance HasId Def where
   getId (DefPrim n _ _) = n
   getId (DefCore n _)   = n
   changeId f (DefPrim n b g) = DefPrim (f n) b g
   changeId f (DefCore n g)   = DefCore (f n) g

instance Show a => Show (GCore a) where
   show core = 
      case core of 
         Apply d xs -> showId d ++ par (map show xs)
         Let ds a   -> "let " ++ concatMap f ds ++ " in " ++ show a 
         Label l a  -> show l ++ ":" ++ show a
         Sym a      -> show a
         Var n      -> show n
    where
      f (n, s) = show n ++ " " ++ show s
      par xs 
         | null xs   = ""
         | otherwise = "(" ++ intercalate ", " xs ++ ")"
 
instance Choice GCore where
   empty  = applyDef0 failDef
   single = Sym
   (<|>)  = applyDef2 choiceDef
   (>|>)  = applyDef2 preferenceDef
   (|>)   = applyDef2 orelseDef

instance Sequence GCore where
   done   = applyDef0 succeedDef
   a ~> s = Sym a <*> s
   (<*>)  = applyDef2 sequenceDef

-----------------------------------------------------------------
-- Useful instances

instance Functor GCore where
   fmap f = rec
    where
      rec core =
         case core of
            Apply d xs -> Apply d (map rec xs)
            Let ds a   -> Let (map (mapSecond rec) ds) (rec a)
            Label l a  -> Label l (rec a)
            Sym a      -> Sym (f a)
            Var n      -> Var n

instance Foldable GCore where
   foldMap f = rec
    where
      rec core =
         case core of
            Apply _ xs -> foldMap rec xs
            Let ds a   -> foldMap (rec . snd) ds <> rec a
            Label _ a  -> rec a
            Sym a      -> f a
            Var _      -> mempty

instance Uniplate (GCore a) where
   uniplate core =
      case core of
         Label l a  -> plate Label  |- l |* a
         Apply d xs -> plate (Apply d) ||* xs
         Let ds a   -> let (ns, bs) = unzip ds
                           make     = Let . zip ns
                       in plate make ||* bs |* a
         _          -> plate core

-----------------------------------------------------------------
-- Definitions

succeedDef :: Def
succeedDef = makeDef "succeed" (const done)

failDef :: Def
failDef = makeDef "fail" (const empty)

sequenceDef :: Def
sequenceDef = associativeDef "sequence" sequence

choiceDef :: Def
choiceDef = associativeDef "choice" choice
        
preferenceDef :: Def
preferenceDef = associativeDef "preference" preference

orelseDef :: Def
orelseDef = associativeDef (newId "orelse") orelse

coreDefs :: [Def]
coreDefs =
   [succeedDef, failDef, sequenceDef, choiceDef, preferenceDef, orelseDef]

-----------------------------------------------------------------

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
      
--------------------------------------------------------------------------------
-- Step

-- | The steps during the parsing process: enter (or exit) a labeled
-- sub-strategy, or a rule.
data Step a = Enter Id                      -- ^ Enter a labeled sub-strategy
            | Exit Id                       -- ^ Exit a labeled sub-strategy
            | RuleStep Environment (Rule a) -- ^ Rule that was applied
   deriving Eq

instance Show (Step a) where
   show (Enter l) = "enter " ++ showId l
   show (Exit l)  = "exit " ++ showId l
   show (RuleStep _ r) = show r

instance Apply Step where
   applyAll (RuleStep _ r) = applyAll r
   applyAll _              = return

instance HasId (Step a) where
   getId (Enter l)      = getId l
   getId (Exit l)       = getId l
   getId (RuleStep _ r) = getId r

   changeId f (Enter l)        = Enter (changeId f l)
   changeId f (Exit l)         = Exit  (changeId f l)
   changeId f (RuleStep env r) = RuleStep env (changeId f r)

instance Minor (Step a) where
   setMinor b (RuleStep env r) = RuleStep env (setMinor b r)
   setMinor _ st = st

   isMinor (RuleStep _ r) = isMinor r
   isMinor _ = True

instance AtomicSymbol (Step a) where
   atomicOpen  = RuleStep mempty (idRule "atomic.open")
   atomicClose = RuleStep mempty (idRule "atomic.close")

instance LabelSymbol (Step a) where
   isEnter (Enter _) = True
   isEnter _         = False

stepRule :: Step a -> Rule a
stepRule (RuleStep _ r) = r
stepRule (Enter l)      = idRule (l # "enter")
stepRule (Exit l)       = idRule (l # "exit")

stepEnvironment :: Step a -> Environment
stepEnvironment (RuleStep env _) = env
stepEnvironment _ = mempty