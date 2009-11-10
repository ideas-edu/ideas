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
   , strategyTree, runTree --, makeTree 
   , mapRule, coreVars, noLabels, mapCore, mapCoreM --, catMaybeLabel --, , 
   , mapLabel, Translation, ForLabel(..) --, simpleTranslation
   ) where

import qualified Common.Strategy.Grammar as Grammar
import Common.Strategy.Grammar (Grammar, (<*>), (<|>), symbol)
import Common.Apply
import Common.Derivation
import Common.Transformation
import Common.Uniplate
import Control.Monad.Identity

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
   | Many (Core l a)
   | Not (Core () a) -- proves that there are no labels inside
   | Label l (Core l a)
   | Succeed
   | Fail
   | Rule (Maybe l) (Rule a)
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
         Label l a -> ([a],   \[x]   -> Label l x)
         Rec n a   -> ([a],   \[x]   -> Rec n x)
         Not a     -> ([noLabels a], \[x] -> Not (noLabels x))
         _         -> ([],    \_     -> core)

-----------------------------------------------------------------
-- The strategy tree (static, no term)

strategyTree :: Translation l a b -> Core l a -> DerivationTree b ()
strategyTree t = grammarTree . toGrammar t

grammarTree :: Grammar a -> DerivationTree a ()
grammarTree gr = addBranches list node
 where 
   node = singleNode () (Grammar.empty gr)
   list = [ (f, grammarTree rest) | (f, rest) <- Grammar.firsts gr ]

-----------------------------------------------------------------
-- Running a strategy

makeTree :: Core l a -> a -> DerivationTree (Rule a) a
makeTree c = changeLabel fst . runTree (strategyTree simpleTranslation c)

runTree :: Apply f => DerivationTree (f a) info -> a -> DerivationTree (f a, info) a
runTree t a = addBranches list (singleNode a (endpoint t))
 where
   list = concatMap make (branches t)
   make (f, st) = [ ((f, root st), runTree st b) | b <- applyAll f a ]

-----------------------------------------------------------------
-- Translation to Grammar data type

type Translation l a b = (l -> ForLabel b, Rule a -> b)

data ForLabel a = Skip | Before a | After a | Around a a

simpleTranslation :: Translation l a (Rule a)
simpleTranslation = (const Skip, id)

toGrammar :: Translation l a b -> Core l a -> Grammar b
toGrammar (f, g) = rec
 where
   rec core =
      case core of
         a :*: b   -> rec a <*> rec b
         a :|: b   -> rec a <|> rec b
         a :|>: b  -> rec (a :|: (Not (noLabels a) :*: b))
         Many a    -> Grammar.many (rec a)
         Succeed   -> Grammar.succeed
         Fail      -> Grammar.fail
         Label l a -> forLabel l (rec a)
         Rule ml r -> (maybe id forLabel ml) (symbol (g r))
         Var n     -> Grammar.var n
         Rec n a   -> Grammar.rec n (rec a)
         Not a     -> symbol (g (notRule a))
   
   forLabel l g =
      case f l of
         Skip       -> g
         Before s   -> symbol s <*> g
         After    t -> g <*> symbol t
         Around s t -> symbol s <*> g <*> symbol t

notRule :: Apply f => f a -> Rule a
notRule f = checkRule (not . applicable f)
   
-----------------------------------------------------------------
-- Utility functions

mapLabel :: (l -> m) -> Core l a -> Core m a
mapLabel f = mapCore (Label . f) (Rule . fmap f)

mapRule :: (Rule a -> Rule b) -> Core l a -> Core l b
mapRule f = mapCore Label (\ml -> Rule ml . f)

noLabels :: Core l a -> Core m a
noLabels = mapCore (const id) (const (Rule Nothing))
   
catMaybeLabel :: Core (Maybe l) a -> Core l a
catMaybeLabel = mapCore (maybe id Label) (Rule . join)
   
mapCore :: (l -> Core m b -> Core m b) -> (Maybe l -> Rule a -> Core m b) 
        -> Core l a -> Core m b
mapCore f g = 
   let fm l = return . f l . runIdentity
       gm l = return . g l
   in runIdentity . mapCoreM fm gm

-- The most primitive function that applies functions to the label and 
-- rule alternatives. Monadic version.
mapCoreM :: Monad m => (k -> m (Core l b) -> m (Core l b)) 
                   -> (Maybe k -> Rule a -> m (Core l b)) 
                   -> Core k a -> m (Core l b)
mapCoreM f g = rec 
 where 
   rec core =
      case core of
         a :*: b   -> liftM2 (:*:)  (rec a) (rec b)
         a :|: b   -> liftM2 (:|:)  (rec a) (rec b)
         a :|>: b  -> liftM2 (:|>:) (rec a) (rec b)
         Many a    -> liftM Many (rec a)
         Succeed   -> return Succeed
         Fail      -> return Fail
         Label l a -> f l (rec a)
         Rule ml r -> g ml r
         Var n     -> return (Var n)
         Rec n a   -> liftM (Rec n) (rec a)
         Not a     -> do 
            let recNot h = mapCoreM (const id) (const h)
            b <- recNot (g Nothing) a
            c <- recNot (return . Rule Nothing) b
            return (Not c)
      
coreVars :: Core l a -> [Int]
coreVars s = [ n | Rec n _ <- universe s ] ++ [ n | Var n <- universe s ]
