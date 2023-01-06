{-# LANGUAGE TypeFamilies, RankNTypes #-}
-----------------------------------------------------------------------------
-- Copyright 2019, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Abstract data type for a 'Strategy' and a 'LabeledStrategy'.
--
-----------------------------------------------------------------------------

module Ideas.Common.Strategy.Abstract
   ( -- * Strategy data type
     Strategy
     -- * Labeled strategies
   , LabeledStrategy, label, unlabel
     -- * Lifting to strategies
   , IsStrategy(..), liftS, liftS2, liftSn
    -- * Prefixes
   , emptyPrefix, replayPath, replayPaths, replayStrategy
    -- * Rules
   , rulesInStrategy, mapRules, mapRulesS
   , cleanUpStrategy, cleanUpStrategyAfter
   , derivationList
     -- * Access to underlying representation
   , toStrategyTree, onStrategyTree
     -- * Strategy declarations
   , useDecl, decl0, decl1, decl2, declN
   ) where

import Data.Foldable (toList)
import Data.Maybe
import Ideas.Common.Classes
import Ideas.Common.Derivation
import Ideas.Common.Environment
import Ideas.Common.Id
import Ideas.Common.Rewriting (RewriteRule)
import Ideas.Common.Rule
import Ideas.Common.Strategy.Choice
import Ideas.Common.Strategy.CyclicTree hiding (label)
import Ideas.Common.Strategy.Prefix
import Ideas.Common.Strategy.Process
import Ideas.Common.Strategy.Sequence (Sequence(..), ready)
import Ideas.Common.Strategy.StrategyTree
import Ideas.Common.View
import Prelude hiding (sequence)
import qualified Ideas.Common.Strategy.CyclicTree as Tree

-----------------------------------------------------------
--- Strategy data-type

-- | Abstract data type for strategies
newtype Strategy a = S { unS :: StrategyTree a }

instance Show (Strategy a) where
   show = show . unS

instance Apply Strategy where
   applyAll = runProcess . getProcess

instance Choice (Strategy a) where
   empty   = decl0 ("fail" .=. Nullary empty)
   s .|. t = choice [s, t]

   s |>  t = orelse [s, t]
   s ./. t = preference [s, t]

   choice     = declN (associative ("choice" .=. Nary choice))
   preference = declN (associative ("preference" .=. Nary preference))
   orelse     = declN (associative ("orelse" .=. Nary orelse))

instance Sequence (Strategy a) where
   type Sym (Strategy a) = Rule a

   done     = decl0 ("succeed" .=. Nullary done)
   a ~> s   = sequence [toStrategy a, s]
   s .*. t  = sequence [s, t]
   single   = toStrategy
   sequence = declN (associative ("sequence" .=. Nary sequence))

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
   toStrategy = S . leaf . LeafRule

instance IsStrategy RewriteRule where
   toStrategy = toStrategy . ruleRewrite

instance IsStrategy Dynamic where
   toStrategy = S . leaf . LeafDyn

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
replayStrategy :: IsStrategy f => Path -> f a -> a -> Maybe (a, Prefix a)
replayStrategy path s a = do
   let (xs, f) = replayProcess path (getProcess s)
   b <- applyList xs a
   return (b, f b)

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
rulesInStrategy s = concatMap f (toList (toStrategyTree s))
 where
   f (LeafRule r) | isMajor r = [r]
   f _ = []

instance Lift LabeledStrategy where
   liftWithM f (LS n s) = LS n (liftWithM f s)

instance Lift Strategy where
   liftWithM f = S . fmap (liftWithM f) . toStrategyTree

-- | Apply a function to all the rules that make up a labeled strategy
mapRules :: (Rule a -> Rule a) -> LabeledStrategy a -> LabeledStrategy a
mapRules f (LS n s) = LS n (mapRulesS f s)

mapRulesS :: (Rule a -> Rule a) -> Strategy a -> Strategy a
mapRulesS = onStrategyTree . mapRulesInTree

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

getProcess :: IsStrategy f => f a -> Process (Leaf a)
getProcess = treeToProcess . toStrategyTree

-------------------------

decl0 :: Decl Nullary -> Strategy a
decl0 = fromNullary . useDecl

decl1 :: IsStrategy f => Decl Unary -> f a -> Strategy a
decl1 = liftS . fromUnary . useDecl

decl2 :: (IsStrategy f, IsStrategy g) => Decl Binary -> f a -> g a -> Strategy a
decl2 = liftS2 . fromBinary . useDecl

declN :: IsStrategy f => Decl Nary -> [f a] -> Strategy a
declN = liftSn . fromNary . useDecl

useDecl :: Arity f => Decl f -> f (Strategy a)
useDecl = liftIso (S <-> unS) . applyDecl