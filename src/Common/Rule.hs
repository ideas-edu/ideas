-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- A rule is just a transformation with some meta-information, such as a name 
-- (which should be unique) and properties such as "buggy" or "minor". Rules
-- can be lifted with a view using the LiftView type class. 
--
-----------------------------------------------------------------------------
module Common.Rule
   ( -- * Rules
     Rule, applyRule, isMinorRule, isMajorRule, isBuggyRule, isRewriteRule
   , finalRule, isFinalRule, ruleSiblings, rule, ruleList
   , makeRule, makeSimpleRule, makeSimpleRuleList
   , idRule, checkRule, emptyRule, minorRule, buggyRule, doAfter
   , siblingOf, useEquality, ruleEquality, ruleRecognizer, getRewriteRules, getUsedRefs
   ) where

import Common.Environment
import Common.Classes
import Common.Id
import Common.Rewriting
import Common.Transformation
import Common.Utils
import Common.View
import Control.Arrow
import Control.Monad
import Data.Foldable
import Data.Function
import Data.Maybe
import Data.Monoid
import Test.QuickCheck

-----------------------------------------------------------
--- Rules

-- | Abstract data type for representing rules
data Rule a = Rule
   { ruleId       :: Id  -- ^ Unique identifier of the rule
   , ruleTrans    :: Transformation a
   , isBuggyRule  :: Bool -- ^ Inspect whether or not the rule is buggy (unsound)
   , isMinorRule  :: Bool -- ^ Returns whether or not the rule is minor (i.e., an administrative step that is automatically performed by the system)
   , isFinalRule  :: Bool -- ^ Final (clean-up) step in derivation
   , ruleSiblings :: [Id]
   , ruleEquality :: Maybe (a -> a -> Bool)
   }

instance Show (Rule a) where
   show = showId

instance Eq (Rule a) where
   r1 == r2 = ruleId r1 == ruleId r2

instance Ord (Rule a) where
   compare = compareId

instance Apply Rule where
   applyAll r = map fst . applyRule r

applyRule :: Rule a -> a -> [(a, Environment)]
applyRule = applyTransformation . ruleTrans

instance HasId (Rule a) where
   getId        = ruleId
   changeId f r = r { ruleId = f (ruleId r) }

instance LiftView Rule where
   liftViewIn v r = r
      { ruleTrans    = transMaybe (match v) >>> first (ruleTrans r) >>> arr (build v)
      , ruleEquality = fmap liftEq (ruleEquality r)
      }
    where
       liftEq eq x y = fromMaybe False $ 
          liftM2 (on eq fst) (match v x) (match v y)

instance (Arbitrary a, CoArbitrary a) => Arbitrary (Rule a) where
   arbitrary = liftM3 make arbitrary arbitrary arbitrary
    where
      make :: Bool -> Id -> (a -> Maybe a) -> Rule a
      make minor n f
         | minor     = minorRule $ makeSimpleRule n f
         | otherwise = makeSimpleRule n f

-- | Returns whether or not the rule is major (i.e., not minor)
isMajorRule :: Rule a -> Bool
isMajorRule = not . isMinorRule

isRewriteRule :: Rule a -> Bool
isRewriteRule = not . null . transRewriteRules . ruleTrans

siblingOf :: HasId b => b -> Rule a -> Rule a
siblingOf sib r = r { ruleSiblings = getId sib : ruleSiblings r }

ruleList :: (IsId n, RuleBuilder f a) => n -> [f] -> Rule a
ruleList n = makeRule a . mconcat . map (makeRewriteTrans . rewriteRule a)
 where a = newId n

rule :: (IsId n, RuleBuilder f a) => n -> f -> Rule a
rule n = makeRule a . makeRewriteTrans . rewriteRule a
 where a = newId n

-- | Turn a transformation into a rule: the first argument is the rule's name
makeRule :: IsId n => n -> Transformation a -> Rule a
makeRule n t = Rule (newId n) t False False False [] Nothing

-- | Turn a function (which returns its result in the Maybe monad) into a rule: 
-- the first argument is the rule's name
makeSimpleRule :: IsId n => n -> (a -> Maybe a) -> Rule a
makeSimpleRule = makeSimpleRuleList

-- | Turn a function (which returns a list of results) into a rule: the first 
-- argument is the rule's name
makeSimpleRuleList :: (IsId n, Foldable f) => n -> (a -> f a) -> Rule a
makeSimpleRuleList n = makeRule n . makeTransG

-- | A special (minor) rule that always returns the identity
idRule :: Rule a
idRule = minorRule $ makeSimpleRule "Identity" Just

-- | A special (minor) rule that checks a predicate (and returns the identity
-- if the predicate holds)
checkRule :: (a -> Bool) -> Rule a
checkRule p = minorRule $ makeSimpleRule "Check" $ \a ->
   if p a then Just a else Nothing

-- | A special (minor) rule that is never applicable (i.e., this rule always fails)
emptyRule :: Rule a
emptyRule = minorRule $ makeSimpleRule "Empty" (const Nothing)

-- | Mark the rule as minor (by default, rules are not minor)
minorRule :: Rule a -> Rule a
minorRule r = r {isMinorRule = True}

-- | Mark the rule as buggy (by default, rules are supposed to be sound)
buggyRule :: Rule a -> Rule a
buggyRule r = r {isBuggyRule = True}

-- | Mark the rule as final (by default, false). Final rules are used as a
-- final step in the derivation, to get the term in the expected form
finalRule :: Rule a -> Rule a
finalRule r = r {isFinalRule = True}

-- | Perform the function after the rule has been fired
doAfter :: (a -> a) -> Rule a -> Rule a
doAfter f r = r {ruleTrans = ruleTrans r >>^ f }

useEquality :: (a -> a -> Bool) -> Rule a -> Rule a
useEquality eq r = r {ruleEquality = Just eq}

ruleRecognizer :: (a -> a -> Bool) -> Rule a -> Recognizer a
ruleRecognizer eq0 r = 
   let eq = fromMaybe eq0 (ruleEquality r)
   in transRecognizer eq (getId r) (ruleTrans r)
   
getRewriteRules :: Rule a -> [Some RewriteRule]
getRewriteRules = transRewriteRules . ruleTrans

getUsedRefs :: Rule a -> IO [Some Ref]
getUsedRefs = transRefs . ruleTrans