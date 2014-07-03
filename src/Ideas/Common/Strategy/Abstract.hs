{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- Copyright 2014, Open Universiteit Nederland. This file is distributed
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
   , rulesInStrategy
   , mapRules, mapRulesS
   , cleanUpStrategy, cleanUpStrategyAfter
     -- Accessors to the underlying representation
   , toCore, fromCore, liftCore, liftCore2, makeLabeledStrategy
   , toLabeledStrategy
   , LabelInfo, processLabelInfo, changeInfo, makeInfo
   , removed, collapsed, hidden, IsLabeled(..), noInterleaving
   ) where

import Control.Monad
import Data.Function
import Data.List
import Ideas.Common.Classes
import Ideas.Common.Derivation
import Ideas.Common.Environment
import Ideas.Common.Id
import Ideas.Common.Rewriting (RewriteRule)
import Ideas.Common.Rule
import Ideas.Common.Strategy.Core
import Ideas.Common.Strategy.Parsing
import Ideas.Common.Utils.Uniplate hiding (rewriteM)
import Ideas.Common.Utils (snd3)
import Ideas.Common.View
import Test.QuickCheck hiding (label)

-----------------------------------------------------------
--- Strategy data-type

-- | Abstract data type for strategies
newtype Strategy a = S { toCore :: Core LabelInfo a }

instance Show (Strategy a) where
   show = show . toCore

instance Apply Strategy where
   applyAll = runCore . toCore

instance (Arbitrary a, CoArbitrary a) => Arbitrary (Strategy a) where
   arbitrary = liftM fromCore arbitrary

-----------------------------------------------------------
--- The information used as label in a strategy

data LabelInfo = Info
   { labelId   :: Id
   , removed   :: Bool
   , collapsed :: Bool
   , hidden    :: Bool
   }
 deriving (Eq, Ord)

instance Show LabelInfo where
   show info =
      let ps = ["removed"   | removed   info] ++
               ["collapsed" | collapsed info] ++
               ["hidden"    | hidden    info]
          extra = " (" ++ intercalate ", " ps ++ ")"
      in showId info ++ if null ps then "" else extra

instance HasId LabelInfo where
   getId = labelId
   changeId f info = info { labelId = f (labelId info) }

instance Arbitrary LabelInfo where
   arbitrary = liftM (makeInfo :: Id -> LabelInfo) arbitrary

makeInfo :: IsId a => a -> LabelInfo
makeInfo s = Info (newId s) False False False

-----------------------------------------------------------
--- Type class

-- | Type class to turn values into strategies
class IsStrategy f where
   toStrategy :: f a -> Strategy a

instance IsStrategy Strategy where
   toStrategy = id

instance IsStrategy (LabeledStrategy) where
  toStrategy (LS info (S core)) = S (Label info core)

instance IsStrategy Rule where
   toStrategy r
      | isMajor r = toStrategy (toLabeled r)
      | otherwise = S (Rule r)

instance IsStrategy RewriteRule where
   toStrategy = toStrategy . ruleRewrite

-----------------------------------------------------------
--- Labeled Strategy data-type

-- | A strategy which is labeled with a string
data LabeledStrategy a = LS
   { labelInfo :: LabelInfo  -- ^ Returns information associated with this label
   , unlabel   :: Strategy a -- ^ Removes the label from a strategy
   }

makeLabeledStrategy :: IsStrategy f => LabelInfo -> f a -> LabeledStrategy a
makeLabeledStrategy info = LS info . toStrategy

toLabeledStrategy :: Monad m => Strategy a -> m (LabeledStrategy a)
toLabeledStrategy s =
   case toCore s of
      Label l c -> return (makeLabeledStrategy l (fromCore c))
      _         -> fail "Strategy without label"

instance Show (LabeledStrategy a) where
   show s = show (labelInfo s) ++ ": " ++ show (unlabel s)

instance Apply LabeledStrategy where
   applyAll = applyAll . toStrategy

instance HasId (LabeledStrategy a) where
   getId = getId . labelInfo
   changeId = changeInfo . changeId

class IsLabeled f where
   toLabeled :: f a -> LabeledStrategy a

instance IsLabeled LabeledStrategy where
   toLabeled = id

instance IsLabeled Rule where
   toLabeled r = LS (makeInfo (getId r)) (S (Rule r))

instance IsLabeled RewriteRule where
   toLabeled = toLabeled . ruleRewrite

-- | Labels a strategy with an identifier. Labels are used to identify
-- substrategies and to specialize feedback messages. The first argument of
-- 'label' can be of type 'String', in which case the string is used as
-- identifier (and not as description).
label :: (IsId l, IsStrategy f) => l -> f a -> LabeledStrategy a
label l = LS (makeInfo l) . toStrategy

changeInfo :: IsLabeled f => (LabelInfo -> LabelInfo) -> f a -> LabeledStrategy a
changeInfo f a = LS (f info) s
 where LS info s = toLabeled a

-----------------------------------------------------------
--- Process Label Information

processLabelInfo :: (l -> LabelInfo) -> Core l a -> Core l a
processLabelInfo getInfo = rec []
 where
   rec env core =
      case core of
         -- Rec n a   -> Rec n (rec ((n, core):env) a)
         -- Let ??
         Label l a -> forLabel env l (rec env a)
         _ -> descend (rec env) core

   forLabel env l c
      | removed info   = Fail
      | collapsed info = Label l (Rule asRule) -- !!
      | otherwise      = new
    where
      new | hidden info = fmap minor (Label l c)
          | otherwise   = Label l c
      info   = getInfo l
      asRule = makeRule (getId info) (runCore (subst new))
      subst  = flip (foldl (flip (uncurry substCoreVar))) env

-----------------------------------------------------------
--- Remaining functions

derivationList :: IsStrategy f => (Rule a -> Rule a -> Ordering) -> f a -> a -> [Derivation (Rule a, Environment) a]
derivationList cmpRule s a0 = rec a0 (toState s)
 where
   toState = majorOnly . makeState a0 . processLabelInfo id . toCore . toStrategy
 
   rec a pst = (if ready pst then (emptyDerivation a:) else id)
      [ prepend (a, rEnv) d | (b, rEnv, new) <- firstsOrd pst, d <- rec b new ]
 
   firstsOrd = sortBy cmp . map f . firsts
    where
      cmp = cmpRule `on` (fst . snd3)
      
      f (b, st) = (b, g (trace st), st)
      
      g (RuleStep env r:_) = (r, env)
      g _ = (emptyRule (), makeEnvironment [])

-- | Returns a list of all major rules that are part of a labeled strategy
rulesInStrategy :: IsStrategy f => f a -> [Rule a]
rulesInStrategy f = [ r | Rule r <- universe (toCore (toStrategy f)), isMajor r ]

instance LiftView LabeledStrategy where
   liftViewIn = mapRules . liftViewIn

instance LiftView Strategy where
   liftViewIn = mapRulesS . liftViewIn

-- | Apply a function to all the rules that make up a labeled strategy
mapRules :: (Rule a -> Rule b) -> LabeledStrategy a -> LabeledStrategy b
mapRules f (LS n s) = LS n (mapRulesS f s)

mapRulesS :: (Rule a -> Rule b) -> Strategy a -> Strategy b
mapRulesS f = S . fmap f . toCore
{-
mapRulesM :: Monad m => (Rule a -> m (Rule a)) -> Strategy a -> m (Strategy a)
mapRulesM f = liftM S . T.mapM f . toCore
-}
-- | Use a function as do-after hook for all rules in a labeled strategy, but
-- also use the function beforehand
cleanUpStrategy :: (a -> a) -> LabeledStrategy a -> LabeledStrategy a
cleanUpStrategy f (LS n s) = cleanUpStrategyAfter f (LS n (make s))
 where
   make = liftCore2 (:*:) (doAfter f (idRule ()))

-- | Use a function as do-after hook for all rules in a labeled strategy
cleanUpStrategyAfter :: (a -> a) -> LabeledStrategy a -> LabeledStrategy a
cleanUpStrategyAfter f = mapRules $ \r ->
   if isMajor r then doAfter f r else r

noInterleaving :: IsStrategy f => f a -> Strategy a
noInterleaving = liftCore $ transform f
   where
      f (a :%:  b) = a :*: b
      f (Atomic a) = a
      f s          = s

-----------------------------------------------------------
--- Functions to lift the core combinators

fromCore :: Core LabelInfo a -> Strategy a
fromCore = S

liftCore :: IsStrategy f => (Core LabelInfo a -> Core LabelInfo a) -> f a -> Strategy a
liftCore f = fromCore . f . toCore . toStrategy

liftCore2 :: (IsStrategy f, IsStrategy g) => (Core LabelInfo a -> Core LabelInfo a -> Core LabelInfo a) -> f a -> g a -> Strategy a
liftCore2 f = liftCore . f . toCore . toStrategy