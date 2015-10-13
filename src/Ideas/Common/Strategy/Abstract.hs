{-# LANGUAGE TypeFamilies, RankNTypes #-}
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
-----------------------------------------------------------------------------
--  $Id$

module Ideas.Common.Strategy.Abstract
   ( Strategy, IsStrategy(..)
   , LabeledStrategy, label, unlabel
   , derivationList
   , emptyPrefix, replayPath, replayPaths, replayStrategy
   , rulesInStrategy, mapRules, mapRulesS
   , cleanUpStrategy, cleanUpStrategyAfter
     -- Accessors to the underlying representation
   , liftS, liftS2, liftSn
   , toStrategyTree, onStrategyTree
   , combinator, combinatorA, combinator1, combinator2
   , Combinator, useCombinator, StrategyTree, useDef
   ) where

import Data.Foldable (toList)
import Data.Function (on)
import Ideas.Common.Classes
import Ideas.Common.Derivation
import Ideas.Common.Environment
import Ideas.Common.Id
import Ideas.Common.Rewriting (RewriteRule)
import Ideas.Common.Rule
import Ideas.Common.CyclicTree hiding (label)
import Ideas.Common.Strategy.Choice
import Ideas.Common.Strategy.Prefix
import Ideas.Common.Strategy.Process
import Ideas.Common.Strategy.Step
import Ideas.Common.Strategy.Sequence (Sequence(..), ready)
import Ideas.Common.View
import Prelude hiding (sequence)
import qualified Ideas.Common.CyclicTree as Tree

-----------------------------------------------------------
--- Strategy data-type

-- | Abstract data type for strategies
newtype Strategy a = S { unS :: StrategyTree a }

instance Show (Strategy a) where
   show = show . unS

instance Apply Strategy where
   applyAll = runProcess . getProcess

instance Choice (Strategy a) where
   empty   = useCombinator (combinator0 "fail" empty)
   s .|. t = choice [s, t]
   
   s |>  t = orelse [s, t]
   s ./. t = preference [s, t]
   
   choice     = useCombinator (combinatorA "choice" choice)
   preference = useCombinator (combinatorA "preference" preference)
   orelse     = useCombinator (combinatorA "orelse" orelse)

instance Sequence (Strategy a) where
   type Sym (Strategy a) = Rule a

   done     = useCombinator (combinator0 "succeed" done)
   a ~> s   = sequence [toStrategy a, s]
   s .*. t  = sequence [s, t]
   single   = toStrategy
   sequence = useCombinator (combinatorA "sequence" sequence)

instance Fix (Strategy a) where
   fix f = S (fix (unS . f . S))
   
-----------------------------------------------------------
--- Type class

-- | Type class to turn values into strategies
class IsStrategy f where
   toStrategy :: f a -> Strategy a

instance IsStrategy Strategy where
   toStrategy = id

instance IsStrategy LabeledStrategy where
  toStrategy (LS info (S t)) = S (Tree.label info t)

instance IsStrategy Rule where
   toStrategy = S . leaf

instance IsStrategy RewriteRule where
   toStrategy = toStrategy . ruleRewrite

liftS :: IsStrategy f => (Strategy a -> Strategy a) -> f a -> Strategy a
liftS f = f . toStrategy

liftS2 :: (IsStrategy f, IsStrategy g) 
       => (Strategy a -> Strategy a -> Strategy a) -> f a -> g a -> Strategy a
liftS2 f = liftS . f . toStrategy

liftSn :: IsStrategy f => ([Strategy a] -> Strategy a) -> [f a] -> Strategy a
liftSn f = f . map toStrategy

-----------------------------------------------------------
--- Labeled Strategy data-type

-- | A strategy which is labeled with an identifier
data LabeledStrategy a = LS Id (Strategy a)

instance Show (LabeledStrategy a) where
   show s = showId s ++ ": " ++ show (unlabel s)

instance Apply LabeledStrategy where
   applyAll = applyAll . toStrategy

instance HasId (LabeledStrategy a) where
   getId (LS l _)      = l
   changeId f (LS l s) = LS (changeId f l) s

-- | Labels a strategy with an identifier. Labels are used to identify
-- substrategies and to specialize feedback messages. The first argument of
-- 'label' can be of type 'String', in which case the string is used as
-- identifier (and not as description).
label :: (IsId l, IsStrategy f) => l -> f a -> LabeledStrategy a
label l = LS (newId l) . toStrategy

-- | Removes the label from a strategy
unlabel :: LabeledStrategy a -> Strategy a
unlabel (LS _ s) = s

-- | Construct the empty prefix for a labeled strategy
emptyPrefix :: IsStrategy f => f a -> a -> Prefix a
emptyPrefix = makePrefix . getProcess

-- | Construct a prefix for a path and a labeled strategy. The third argument
-- is the current term.
replayPath :: IsStrategy f => Path -> f a -> a -> ([Rule a], Prefix a)
replayPath path s a =
   let (xs, f) = replayProcess path (getProcess s)
   in (xs, f a)

-- | Construct a prefix for a list of paths and a labeled strategy. The third
-- argument is the current term.
replayPaths :: IsStrategy f => [Path] -> f a -> a -> Prefix a
replayPaths paths s a = mconcat
   [ snd (replayPath path s a) | path <- paths ]

-- | Construct a prefix for a path and a labeled strategy. The third argument
-- is the initial term.
replayStrategy :: (Monad m, IsStrategy f) => Path -> f a -> a -> m (a, Prefix a)
replayStrategy path s a =
   let (xs, f) = replayProcess path (getProcess s)
   in case applyList xs a of
         Just b  -> return (b, f b)
         Nothing -> fail "Cannot replay strategy"

-----------------------------------------------------------
--- Remaining functions

derivationList :: IsStrategy f => (Rule a -> Rule a -> Ordering) -> f a -> a -> [Derivation (Rule a, Environment) a]
derivationList cmpRule s a0 = rec a0 (toPrefix s)
 where
   toPrefix = majorPrefix . flip makePrefix a0 . getProcess

   rec a prfx = (if ready prfx then (emptyDerivation a:) else id)
      [ prepend (a, rEnv) d | (rEnv, b, new) <- firstsOrd prfx, d <- rec b new ]

   firstsOrd = map f . firstsOrdered cmpRule
    where
      f ((stp, b, env), new) = ((stp, env), b, new)

-- | Returns a list of all major rules that are part of a labeled strategy
rulesInStrategy :: IsStrategy f => f a -> [Rule a]
rulesInStrategy s = [ r | r <- toList (toStrategyTree s), isMajor r ]

instance LiftView LabeledStrategy where
   liftViewIn = mapRules . liftViewIn

instance LiftView Strategy where
   liftViewIn = mapRulesS . liftViewIn

-- | Apply a function to all the rules that make up a labeled strategy
mapRules :: (Rule a -> Rule b) -> LabeledStrategy a -> LabeledStrategy b
mapRules f (LS n s) = LS n (mapRulesS f s)

mapRulesS :: (Rule a -> Rule b) -> Strategy a -> Strategy b
mapRulesS f = S . fmap f . unS

-- | Use a function as do-after hook for all rules in a labeled strategy, but
-- also use the function beforehand
cleanUpStrategy :: (a -> a) -> LabeledStrategy a -> LabeledStrategy a
cleanUpStrategy f (LS n s) = cleanUpStrategyAfter f $ 
   LS n (doAfter f (idRule ()) ~> s)

-- | Use a function as do-after hook for all rules in a labeled strategy
cleanUpStrategyAfter :: (a -> a) -> LabeledStrategy a -> LabeledStrategy a
cleanUpStrategyAfter f = mapRules $ \r ->
   if isMajor r then doAfter f r else r

-----------------------------------------------------------
--- Functions to lift the core combinators

toStrategyTree :: IsStrategy f => f a -> StrategyTree a
toStrategyTree = unS . toStrategy

onStrategyTree :: IsStrategy f => (StrategyTree a -> StrategyTree a) -> f a -> Strategy a
onStrategyTree f = S . f . toStrategyTree

getProcess :: IsStrategy f => f a -> Process (Rule a)
getProcess = foldUnwind emptyAlg
   { fNode  = applyCombinator
   , fLeaf  = single
   , fLabel = \l p -> enterRule l ~> p .*. (exitRule l ~> done)
   } . toStrategyTree

makeC :: IsId n
            => n 
            -> ([StrategyTree a] -> [StrategyTree a]) 
            -> (forall b . [Process (Rule b)] -> Process (Rule b))
            -> Combinator ([Strategy a] -> Strategy a)
makeC n f op = 
   let strCom = C (newId n) op (S . useDef strCom . f . map unS)
   in strCom

combinator :: IsId n
            => n -> (forall b . [Process (Rule b)] -> Process (Rule b))
            -> Combinator ([Strategy a] -> Strategy a)
combinator n = makeC n id

combinatorA :: IsId n
            => n -> (forall b. [Process (Rule b)] -> Process (Rule b)) 
            -> Combinator ([Strategy a] -> Strategy a)
combinatorA n = makeC myId (concatMap g)
 where
   myId = newId n
   g a  = case isNode a of 
             Just (da, as) | getId da == myId -> as
             _ -> [a]

combinator0 :: IsId n 
            => n -> (forall b . Process (Rule b)) 
            -> Combinator (Strategy a)
combinator0 n p = 
   fmap ($ []) (combinator n (const p))

combinator1 :: IsId n
            => n -> (forall b . Process (Rule b) -> Process (Rule b)) 
            -> Combinator (Strategy a -> Strategy a)
combinator1 n op = 
   fmap (\f x -> f [x]) (combinator n (list1 empty op))
 where
   list1 _ f [a] = f a
   list1 b _ _   = b

combinator2 :: IsId n
            => n -> (forall b . Process (Rule b) -> Process (Rule b) -> Process (Rule b)) 
            -> Combinator (Strategy a -> Strategy a -> Strategy a)
combinator2 n op =
   fmap (\f x y -> f [x, y]) (combinator n (list2 empty op))
 where
   list2 _ f [a1, a2] = f a1 a2
   list2 b _ _        = b

-------------------------------------------------------------------------------

data Combinator a = C
   { combinatorId    :: Id
   , applyCombinator :: forall b . [Process (Rule b)] -> Process (Rule b)
   , useCombinator   :: a
   } 

useDef :: Combinator a -> [StrategyTree b] -> StrategyTree b
useDef d = node (d {useCombinator = ()})

type StrategyTree a = CyclicTree (Combinator ()) (Rule a)

instance Show (Combinator a) where
   show = showId

instance Eq (Combinator a) where
   (==) = (==) `on` getId

instance HasId (Combinator a) where
   getId = combinatorId
   changeId f d = d { combinatorId = f (combinatorId d) }
   
instance Functor Combinator where
   fmap f (C n op a) = C n op (f a)