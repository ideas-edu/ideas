{-# OPTIONS -XFlexibleInstances #-}
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
-----------------------------------------------------------------------------
module Common.Strategy.Abstract 
   ( Strategy, IsStrategy(..)
   , LabeledStrategy, strategyName, label, unlabel
   , fullDerivationTree, derivationTree, rulesInStrategy
   , mapRules, cleanUpStrategy
     -- Accessors to the underlying representation
   , toCore, fromCore, liftCore, liftCore2, fixCore
   ) where

import Common.Strategy.Core
import Common.Strategy.BiasedChoice
import Common.Apply
import Common.Rewriting (RewriteRule(..))
import Common.Transformation
import Common.Derivation
import Common.Uniplate

-----------------------------------------------------------
--- Strategy data-type

-- | Abstract data type for strategies
newtype Strategy a = S { toCore :: Core String a }

instance Show (Strategy a) where
   show = show . toCore

instance Apply Strategy where
   applyAll s = results . fullDerivationTree s

-----------------------------------------------------------
--- Type class

-- | Type class to turn values into strategies
class Apply f => IsStrategy f where
   toStrategy :: f a -> Strategy a

instance IsStrategy (Core String) where
   toStrategy = S

instance IsStrategy Strategy where
   toStrategy = id

instance IsStrategy (LabeledStrategy) where
  toStrategy (LS n s) = S (Label n (toCore s))

instance IsStrategy Rule where -- Major rules receive a label
   toStrategy r
      | isMajorRule r = S (Rule (Just (name r)) r)
      | otherwise     = S (Rule Nothing r)

instance IsStrategy RewriteRule where
   toStrategy r = 
      toStrategy (makeRule (ruleName r) (RewriteRule r))

-----------------------------------------------------------
--- Labeled Strategy data-type

-- | A strategy which is labeled with a string
data LabeledStrategy a = LS 
   { strategyName :: String  -- ^ Returns the label of the strategy
   , unlabel :: Strategy a   -- ^ Removes the label from a strategy
   }

instance Show (LabeledStrategy a) where
   show s = strategyName s ++ ": " ++ show (unlabel s)

instance Apply LabeledStrategy where
   applyAll = applyAll . toStrategy

-- | Labels a strategy with a string
label :: IsStrategy f => String -> f a -> LabeledStrategy a
label l = LS l . toStrategy
      
-----------------------------------------------------------
--- Remaining functions

-- | Returns the derivation tree for a strategy and a term, including all
-- minor rules
fullDerivationTree :: IsStrategy f => f a -> a -> DerivationTree (Rule a) a
fullDerivationTree = makeBiasedTree p . toCore .toStrategy 
 where 
   p t = endpoint t || any isMajorRule (annotations t) || any p (subtrees t)

-- | Returns the derivation tree for a strategy and a term with only major rules
derivationTree :: IsStrategy f => f a -> a -> DerivationTree (Rule a) a
derivationTree s = mergeSteps isMajorRule . fullDerivationTree s

-- | Returns a list of all major rules that are part of a labeled strategy
rulesInStrategy :: IsStrategy f => f a -> [Rule a]
rulesInStrategy f = [ r | Rule _ r <- universe (toCore (toStrategy f)), isMajorRule r ]
                    
-- | Apply a function to all the rules that make up a labeled strategy
mapRules :: (Rule a -> Rule b) -> LabeledStrategy a -> LabeledStrategy b
mapRules f (LS n s) = LS n (S (mapRule f (toCore s)))

-- | Use a function as do-after hook for all rules in a labeled strategy
cleanUpStrategy :: (a -> a) -> LabeledStrategy a -> LabeledStrategy a
cleanUpStrategy f (LS n s) = mapRules g (LS n (S core))
 where
   core = Rule Nothing (doAfter f idRule) :*: toCore s
   g r | isMajorRule r = doAfter f r  
       | otherwise     = r
       
-----------------------------------------------------------
--- Functions to lift the core combinators

fromCore :: Core String a -> Strategy a
fromCore = toStrategy

liftCore :: IsStrategy f => (Core String a -> Core String a) -> f a -> Strategy a
liftCore f = fromCore . f . toCore . toStrategy

liftCore2 :: (IsStrategy f, IsStrategy g) => (Core String a -> Core String a -> Core String a) -> f a -> g a -> Strategy a
liftCore2 f = liftCore . f . toCore . toStrategy

fixCore :: (Core l a -> Core l a) -> Core l a
fixCore f = Rec i (f (Var i)) -- disadvantage: function f is applied twice
 where
    s = coreVars (f (Rule Nothing idRule))
    i = if null s then 0 else maximum s + 1