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
module Common.Rule.Abstract
   ( -- * Rule data type and accessors
     Rule, transformation, recognizer
     -- * Constructor functions
   , makeRule, ruleMaybe, ruleList, ruleTrans, ruleRewrite
   , buggyRule, minorRule, rewriteRule, rewriteRules
     -- * Special minor rules
   , idRule, checkRule, emptyRule 
     -- * Rule properties
   , finalRule, isFinalRule
   , ruleSiblings, siblingOf
   , isRewriteRule, isRecognizer, doAfter
     -- * Recognizer
   , addRecognizer, addRecognizerBool
   , addRecognizerList, addTransRecognizer
   ) where

import Common.Environment
import Common.Classes
import Common.Id
import Common.Rewriting
import Common.Rule.Transformation
import Common.Rule.Recognizer
import Common.View
import Control.Arrow
import Control.Monad
import Data.Monoid
import Test.QuickCheck

-----------------------------------------------------------
--- Rule data type and accessors

-- | Abstract data type for representing rules
data Rule a = Rule
   { ruleId        :: Id  -- ^ Unique identifier of the rule
   , getTrans      :: Transformation a
   , getRecognizer :: Recognizer a
   , isBuggyRule   :: Bool -- ^ Inspect whether or not the rule is buggy (unsound)
   , isMinorRule   :: Bool -- ^ Returns whether or not the rule is minor (i.e., an administrative step that is automatically performed by the system)
   , isFinalRule   :: Bool -- ^ Final (clean-up) step in derivation
   , ruleSiblings  :: [Id]
   }

instance Show (Rule a) where
   show = showId

instance Eq (Rule a) where
   r1 == r2 = ruleId r1 == ruleId r2

instance Ord (Rule a) where
   compare = compareId

instance Apply Rule where
   applyAll r = map fst . transApply (transformation r)

instance HasId (Rule a) where
   getId        = ruleId
   changeId f r = r { ruleId = f (ruleId r) }

instance LiftView Rule where
   liftViewIn v r = r
      { getTrans      = transLiftViewIn v (getTrans r)
      , getRecognizer = liftViewIn v (getRecognizer r)
      }

instance Recognizable Rule where
   recognizer = getRecognizer

instance Buggy (Rule a) where
   setBuggy b r = r {isBuggyRule = b}
   isBuggy = isBuggyRule

instance Minor (Rule a) where
   setMinor b r = r {isMinorRule = b}
   isMinor = isMinorRule

instance (Arbitrary a, CoArbitrary a) => Arbitrary (Rule a) where
   arbitrary = liftM3 make arbitrary arbitrary arbitrary
    where
      make :: Bool -> Id -> (a -> Maybe a) -> Rule a
      make b n f = setMinor b $ makeRule n f

transformation :: Rule a -> Transformation a
transformation = getTrans

-----------------------------------------------------------
--- Constructor functions

makeRule :: (IsId n, MakeTrans f) => n -> (a -> f a) -> Rule a
makeRule n = ruleTrans n . makeTrans

ruleMaybe :: IsId n => n -> (a -> Maybe a) -> Rule a
ruleMaybe = makeRule

ruleList :: IsId n => n -> (a -> [a]) -> Rule a
ruleList = makeRule

ruleTrans :: IsId n => n -> Transformation a -> Rule a
ruleTrans n f = Rule (newId n) f mempty False False False []

ruleRewrite :: RewriteRule a -> Rule a
ruleRewrite r = ruleTrans (getId r) (transRewrite r)

rewriteRule :: (IsId n, RuleBuilder f a) => n -> f -> Rule a
rewriteRule n = rewriteRules n . return

rewriteRules :: (IsId n, RuleBuilder f a) => n -> [f] -> Rule a
rewriteRules n = 
   let a = newId n
   in ruleTrans a . mconcat . map (transRewrite . makeRewriteRule a)
   
buggyRule :: (IsId n, MakeTrans f) => n -> (a -> f a) -> Rule a
buggyRule n = buggy . makeRule n

minorRule :: (IsId n, MakeTrans f) => n -> (a -> f a) -> Rule a
minorRule n = minor . makeRule n

-----------------------------------------------------------
--- Special minor rules

-- | A special (minor) rule that is never applicable (i.e., this rule always fails)
emptyRule :: IsId n => n -> Rule a
emptyRule n = minor $ ruleTrans n zeroArrow

-- | A special (minor) rule that always returns the identity
idRule :: IsId n => n -> Rule a
idRule n = minor $ ruleTrans n identity

-- | A special (minor) rule that checks a predicate (and returns the identity
-- if the predicate holds)
checkRule :: IsId n => n -> (a -> Bool) -> Rule a
checkRule n p = minorRule n $ \a -> [ a | p a ]

-----------------------------------------------------------
--- Rule properties

isRewriteRule :: Rule a -> Bool
isRewriteRule = not . null . getRewriteRules . transformation

isRecognizer :: Rule a -> Bool
isRecognizer = isZeroTrans . transformation

siblingOf :: HasId b => b -> Rule a -> Rule a
siblingOf sib r = r { ruleSiblings = getId sib : ruleSiblings r }

-- | Mark the rule as final (by default, false). Final rules are used as a
-- final step in the derivation, to get the term in the expected form
finalRule :: Rule a -> Rule a
finalRule r = r {isFinalRule = True}

-- | Perform the function after the rule has been fired
doAfter :: (a -> a) -> Rule a -> Rule a
doAfter f r = r {getTrans = getTrans r >>^ f }

-----------------------------------------------------------
--- Recognizer
   
addRecognizer :: Recognizer a -> Rule a -> Rule a 
addRecognizer a r = r {getRecognizer = a `mappend` getRecognizer r}

addRecognizerBool :: (a -> a -> Bool) -> Rule a -> Rule a
addRecognizerBool eq = addRecognizer (makeRecognizerBool eq)

addRecognizerList :: (a -> a -> [Environment]) -> Rule a -> Rule a
addRecognizerList eq = addRecognizer (makeRecognizerList eq)

addTransRecognizer :: (a -> a -> Bool) -> Rule a -> Rule a
addTransRecognizer eq r = flip addRecognizer r $ 
   makeRecognizerList $ \a b -> do
      (x, env) <- transApply (transformation r) a
      guard (x `eq` b)
      return env