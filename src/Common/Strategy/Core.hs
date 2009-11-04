-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
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
     -- temp!
   , checkRule, makeTree, makeTreeWith, notRule, toGrammar
   , mapRule, mapCore, noLabels, coreVars, collectS
   ) where

import qualified Common.Strategy.Grammar as Grammar
import Common.Strategy.Grammar (Grammar)
import Common.Apply
import Common.Derivation
import Common.Transformation hiding (checkRule)
import Common.Uniplate

-----------------------------------------------------------------
-- Strategy (internal) data structure, containing a selection
-- of combinators

infixr 3 :|:, :|>:
infixr 5 :*:

data Core l a
   = Core l a :*:  Core l a
   | Core l a :|:  Core l a
   | Core l a :|>: Core l a
   | Many (Core l a)
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

instance Apply (Core l) where 
   applyAll core = results . makeTree core

instance Uniplate (Core l a) where
   uniplate core =
      case core of
         a :*: b   -> ([a,b], \[x,y] -> x :*: y)
         a :|: b   -> ([a,b], \[x,y] -> x :|: y)
         a :|>: b  -> ([a,b], \[x,y] -> x :|>: y)
         Many a    -> ([a],   \[x]   -> Many x)
         Not a     -> ([a],   \[x]   -> Not x)
         Label l a -> ([a],   \[x]   -> Label l x)
         Rec n a   -> ([a],   \[x]   -> Rec n x)
         _         -> ([],    \_     -> core)

-----------------------------------------------------------------
-- Running a strategy

makeTree :: Core l a -> a -> DerivationTree (Rule a) a
makeTree = makeTreeWith simpleTranslation
               
makeTreeWith :: Apply f => Translation f l a -> Core l a -> a -> DerivationTree (f a) a
makeTreeWith t = rec . toGrammar t
 where
   rec  gr a = addBranches (list gr a) (singleNode a (Grammar.empty gr))
   list gr a = [ (f, rec rest b)
               | (f, rest) <- Grammar.firsts gr
               , b <- applyAll f a
               ]

-----------------------------------------------------------------
-- Translation to Grammar data type

type Translation f l a =
   (l -> Grammar (f a) -> Grammar (f a), Rule a -> Grammar (f a))

simpleTranslation :: Translation Rule l a
simpleTranslation = (const id, Grammar.symbol)

toGrammar :: Translation f l a -> Core l a -> Grammar (f a)
toGrammar (forLabel, forRule) = rec
 where
   rec core =
      case core of
         a :*: b   -> rec a Grammar.<*> rec b
         a :|: b   -> rec a Grammar.<|> rec b
         a :|>: b  -> rec (a :|: (Not a :*: b))
         Many a    -> Grammar.many (rec a)
         Succeed   -> Grammar.succeed
         Fail      -> Grammar.fail
         Label l a -> forLabel l (rec a)
         Rule r    -> forRule r
         Var n     -> Grammar.var n
         Rec n a   -> Grammar.rec n (rec a)
         Not a     -> forRule (notRule a)
      
notRule :: Apply f => f a -> Rule a
notRule f = checkRule (not . applicable f)
      
checkRule :: (a -> Bool) -> Rule a 
checkRule p = minorRule $ makeSimpleRule "check" $ \a ->
   if p a then Just a else Nothing
   
-----------------------------------------------------------------
-- Utility functions

mapRule :: (Rule a -> Rule b) -> Core l a -> Core l b
mapRule f = mapCore Label (Rule . f)

noLabels :: Core l a -> Core m a
noLabels = mapCore (const id) Rule
   
mapCore :: (l -> Core m b -> Core m b) -> (Rule a -> Core m b) -> Core l a -> Core m b
mapCore f g = rec 
 where 
   rec core =
      case core of
         a :*: b   -> rec a :*:  rec b
         a :|: b   -> rec a :|:  rec b
         a :|>: b  -> rec a :|>: rec b
         Many a    -> Many (rec a)
         Succeed   -> Succeed
         Fail      -> Fail
         Label l a -> f l (rec a)
         Rule r    -> g r
         Var n     -> Var n
         Rec n a   -> Rec n (rec a)
         Not a     -> Not (rec a)
         
coreVars :: Core l a -> [Int]
coreVars s = [ n | Rec n _ <- universe s ] ++ [ n | Var n <- universe s ]

-- For now: just major rules??
collectS :: Core l a -> [Either (l, Core l a) (Rule a)]
collectS (Rule r) = [Right r | isMajorRule r]
collectS (Label l a) = [Left (l, a)]
collectS (Not _) = [] -- [Right $ checkRule (Prelude.not . applicable (S a))]
collectS s = concatMap collectS (children s)