{-# LANGUAGE GADTs, ExistentialQuantification #-} 
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
-- This module defines transformations. Given a term, a transformation returns a list of 
-- results (often a singleton list or the empty list). A transformation can be parameterized
-- with one or more arguments. A rule is in essence just a transformation with a name (which 
-- should be unique). Both transformations and rules can be lifted to work on more complex domains. 
--
-----------------------------------------------------------------------------
module Common.Transformation 
   ( -- * Transformations
     Transformation, makeTrans, makeTransList, makeRewriteTrans
     -- * Arguments
   , ArgDescr(..), defaultArgDescr, Argument(..), ArgValue(..), ArgValues
   , supply1, supply2, supply3
   , hasArguments, expectedArguments, getDescriptors, useArguments
     -- * Rules
   , Rule, isMinorRule, isMajorRule, isBuggyRule, isRewriteRule
   , ruleSiblings, rule, ruleList
   , makeRule, makeRuleList, makeSimpleRule, makeSimpleRuleList
   , idRule, checkRule, emptyRule, minorRule, buggyRule, doAfter
   , siblingOf, transformations, getRewriteRules
   , ruleRecognizer, useRecognizer, useSimpleRecognizer
     -- * Lifting
   , liftRule, liftTrans, liftRuleIn, liftTransIn
     -- * QuickCheck
   , testRule, propRuleSmart
   ) where

import Common.Classes
import Common.Id
import Common.Rewriting
import Common.Utils
import Common.View
import Control.Monad
import Data.Maybe
import Test.QuickCheck

-----------------------------------------------------------
--- Transformations

-- | Abstract data type for representing transformations
data Transformation a
   = Function (a -> [a])
   | RewriteRule (RewriteRule a) (a -> [a])
   | forall b . Abstraction (ArgumentList b) (a -> Maybe b) (b -> Transformation a)
   | forall b c . LiftView (View a (b, c)) (Transformation b)
   | Recognizer (a -> a -> Maybe ArgValues) (Transformation a)
   
instance Apply Transformation where
   applyAll (Function f)        = f
   applyAll (RewriteRule _ f)   = f
   applyAll (Abstraction _ f g) = \a -> maybe [] (\b -> applyAll (g b) a) (f a)
   applyAll (LiftView v t)      = \a -> [ build v (b, c) | (b0, c) <- matchM v a, b <- applyAll t b0  ]
   applyAll (Recognizer _ t)    = applyAll t
   
-- | Turn a function (which returns its result in the Maybe monad) into a transformation 
makeTrans :: (a -> Maybe a) -> Transformation a
makeTrans f = makeTransList (maybe [] return . f)

-- | Turn a function (which returns a list of results) into a transformation 
makeTransList :: (a -> [a]) -> Transformation a
makeTransList = Function

-- | Turn a rewrite rule into a transformation
makeRewriteTrans :: RewriteRule a -> Transformation a
makeRewriteTrans r = RewriteRule r (rewriteM r)

-----------------------------------------------------------
--- Arguments

-- | A data type for describing an argument of a parameterized transformation
data ArgDescr a = ArgDescr
   { labelArgument    :: String               -- ^ Label that is shown to the user when asked to supply the argument
   , defaultArgument  :: Maybe a              -- ^ Default value that can be used
   , parseArgument    :: String -> Maybe a    -- ^ A parser 
   , showArgument     :: a -> String          -- ^ A pretty-printer
   , termViewArgument :: View Term a          -- ^ Conversion to/from term
   , genArgument      :: Gen a                -- ^ An arbitrary argument generator
   }

-- | An argument descriptor, paired with a value
data ArgValue = forall a . ArgValue (ArgDescr a) a

-- | List of argument values
type ArgValues = [ArgValue]

instance Show ArgValue where
   show (ArgValue descr a) = showArgument descr a

instance Eq ArgValue where
   ArgValue d1 a1 == ArgValue d2 a2 = 
      build (termViewArgument d1) a1 == build (termViewArgument d2) a2

-- | Constructor function for an argument descriptor that uses the Show and Read type classes
defaultArgDescr :: (Show a, Read a, IsTerm a, Arbitrary a) => String -> ArgDescr a
defaultArgDescr descr = ArgDescr descr Nothing readM show termView arbitrary

-- | A type class for types which have an argument descriptor
class Arbitrary a => Argument a where
   makeArgDescr :: String -> ArgDescr a   -- ^ The first argument is the label of the argument descriptor

instance Argument Int where
   makeArgDescr = defaultArgDescr

-- | Parameterization with one argument using the provided label
supply1 :: Argument x 
                  => String -> (a -> Maybe x)
                  -> (x -> Transformation a) -> Transformation a
supply1 s f t = 
   let args = Single (makeArgDescr s)
   in Abstraction args f t
   
-- | Parameterization with two arguments using the provided labels
supply2 :: (Argument x, Argument y) 
                   => (String, String) -> (a -> Maybe (x, y)) 
                   -> (x -> y -> Transformation a) -> Transformation a
supply2 (s1, s2) f t = 
   let args = Pair (Single (makeArgDescr s1)) (Single (makeArgDescr s2))
   in Abstraction args f (uncurry t)

-- | Parameterization with three arguments using the provided labels
supply3 :: (Argument x, Argument y, Argument z) 
                  => (String, String, String) -> (a -> Maybe (x, y, z)) 
                  -> (x -> y -> z -> Transformation a) -> Transformation a
supply3 (s1, s2, s3) f t =
   let args = Pair (Single (makeArgDescr s1))
                   (Pair (Single (makeArgDescr s2)) (Single (makeArgDescr s3)))
       nest (a, b, c) = (a, (b, c))
   in Abstraction args (fmap nest . f) (\(a, (b, c)) -> t a b c)

-- | Checks whether a rule is parameterized
hasArguments :: Rule a -> Bool
hasArguments = not . null . getDescriptors

-- | Returns a list of argument descriptors
getDescriptors :: Rule a -> [Some ArgDescr]
getDescriptors r =
   case transformations r of
      [t] -> rec t
      _   -> []
 where 
   rec :: Transformation a -> [Some ArgDescr]
   rec trans = 
      case trans of
         Abstraction args _ _ -> someArguments args
         LiftView _ t   -> rec t
         Recognizer _ t -> rec t
         _ -> []

-- | Returns a list of pretty-printed expected arguments. 
-- Nothing indicates that there are no such arguments (or the arguments
-- are not applicable for the current value)
expectedArguments :: Rule a -> a -> Maybe ArgValues
expectedArguments r =
   case transformations r of
      [t] -> rec t
      _   -> const Nothing
 where
    rec :: Transformation a -> a -> Maybe ArgValues
    rec trans a =  
       case trans of
          Abstraction args f _ -> 
             fmap (argumentValues args) (f a)
          LiftView v t -> do 
             (b, _) <- match v a
             rec t b
          Recognizer _ t ->
             rec t a
          _ -> Nothing

-- | Transform a rule and use a list of pretty-printed arguments. Nothing indicates that the arguments are 
-- invalid (not parsable), or that the wrong number of arguments was supplied
useArguments :: [String] -> Rule a -> Maybe (Rule a)
useArguments list r =
   case transformations r of
      [t] -> do new <- make t
                return r {transformations = [new]}
      _   -> Nothing
 where   
   make :: Transformation a -> Maybe (Transformation a)
   make trans = 
      case trans of
         Abstraction args _ g -> fmap g (parseArguments args list)
         LiftView v t         -> fmap (LiftView v) (make t)
         Recognizer f t       -> fmap (Recognizer f) (make t)
         _                    -> Nothing
   
-----------------------------------------------------------
--- Internal machinery for arguments
               
data ArgumentList a where
   Single :: ArgDescr a -> ArgumentList a
   Pair   :: ArgumentList a -> ArgumentList b -> ArgumentList (a, b)

parseArguments :: ArgumentList a -> [String] -> Maybe a
parseArguments (Single a) [x] = parseArgument a x
parseArguments (Pair a b) xs =
   let (ys, zs) = splitAt (numberOfArguments a) xs 
   in liftM2 (,) (parseArguments a ys) (parseArguments b zs)
parseArguments _ _ = Nothing
   
someArguments :: ArgumentList a -> [Some ArgDescr]
someArguments (Single a) = [Some a]
someArguments (Pair a b) = someArguments a ++ someArguments b

argumentValues :: ArgumentList a -> a -> ArgValues
argumentValues (Single a) x      = [ArgValue a x]
argumentValues (Pair a b) (x, y) = argumentValues a x ++ argumentValues b y

numberOfArguments :: ArgumentList a -> Int
numberOfArguments = length . someArguments
      
-----------------------------------------------------------
--- Rules

-- | Abstract data type for representing rules
data Rule a = Rule 
   { ruleId          :: Id  -- ^ Unique identifier of the rule
   , transformations :: [Transformation a]
   , afterwards      :: a -> a
   , isBuggyRule     :: Bool -- ^ Inspect whether or not the rule is buggy (unsound)
   , isMinorRule     :: Bool -- ^ Returns whether or not the rule is minor (i.e., an administrative step that is automatically performed by the system)
   , ruleSiblings    :: [Id]
   }

instance Show (Rule a) where
   show = showId

instance Eq (Rule a) where
   r1 == r2 = ruleId r1 == ruleId r2

instance Ord (Rule a) where
   compare = compareId

instance Apply Rule where
   applyAll r a = do 
      t <- transformations r
      b <- applyAll t a
      return (afterwards r b)

instance HasId (Rule a) where
   getId        = ruleId
   changeId f r = r { ruleId = f (ruleId r) } 

instance (Arbitrary a, CoArbitrary a) => Arbitrary (Rule a) where
   arbitrary = liftM3 make arbitrary arbitrary arbitrary
    where
      make minor n f
         | minor     = minorRule $ makeSimpleRule n f
         | otherwise = makeSimpleRule (n :: Id) f

-- | Returns whether or not the rule is major (i.e., not minor)
isMajorRule :: Rule a -> Bool
isMajorRule = not . isMinorRule

isRewriteRule :: Rule a -> Bool
isRewriteRule = not . null . getRewriteRules

siblingOf :: HasId b => b -> Rule a -> Rule a 
siblingOf sib r = r { ruleSiblings = getId sib : ruleSiblings r }

ruleList :: (IsId n, RuleBuilder f a) => n -> [f] -> Rule a
ruleList n = makeRuleList a . map (makeRewriteTrans . rewriteRule a)
 where a = newId n
 
rule :: (IsId n, RuleBuilder f a) => n -> f -> Rule a
rule n = makeRule a . makeRewriteTrans . rewriteRule a
 where a = newId n

-- | Turn a transformation into a rule: the first argument is the rule's name
makeRule :: IsId n => n -> Transformation a -> Rule a
makeRule n = makeRuleList n . return

-- | Turn a list of transformations into a single rule: the first argument is the rule's name
makeRuleList :: IsId n => n -> [Transformation a] -> Rule a
makeRuleList n ts = Rule (newId n) ts id False False []

-- | Turn a function (which returns its result in the Maybe monad) into a rule: the first argument is the rule's name
makeSimpleRule :: IsId n => n -> (a -> Maybe a) -> Rule a
makeSimpleRule n = makeRule n . makeTrans

-- | Turn a function (which returns a list of results) into a rule: the first argument is the rule's name
makeSimpleRuleList :: IsId n => n -> (a -> [a]) -> Rule a
makeSimpleRuleList n = makeRule n . makeTransList

-- | A special (minor) rule that always returns the identity
idRule :: Rule a
idRule = minorRule $ makeSimpleRule "Identity" return

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

-- | Perform the function after the rule has been fired
doAfter :: (a -> a) -> Rule a -> Rule a
doAfter f r = r {afterwards = f . afterwards r}

getRewriteRules :: Rule a -> [(Some RewriteRule, Bool)]
getRewriteRules r = concatMap f (transformations r)
 where
   f :: Transformation a -> [(Some RewriteRule, Bool)]
   f trans =
      case trans of
         RewriteRule rr _ -> [(Some rr, not $ isBuggyRule r)]      
         LiftView _ t     -> f t
         _                -> []

ruleRecognizer :: (a -> a -> Bool) -> Rule a -> a -> a -> Maybe ArgValues
ruleRecognizer eq r a b = msum
   [ transRecognizer eq t a b | t <- transformations r ]

transRecognizer :: (a -> a -> Bool) -> Transformation a -> a -> a -> Maybe ArgValues
transRecognizer eq trans a b =
   case trans of
      Recognizer f t -> f a b `mplus` transRecognizer eq t a b
      LiftView v t   -> msum
         [ transRecognizer (\x y -> eq (f x) (f y)) t av bv
         | (av, c) <- matchM v a 
         , (bv, _) <- matchM v b
         , let f z = build v (z, c)
         ] 
       `mplus` 
         noArg (any (`eq` b) (applyAll trans a)) -- is this really needed?
      _ -> noArg $ any (`eq` b) (applyAll trans a)
 where
   noArg c = if c then Just [] else Nothing 

useRecognizer :: (a -> a -> Maybe ArgValues) -> Transformation a -> Transformation a
useRecognizer = Recognizer

useSimpleRecognizer :: (a -> a -> Bool) -> Transformation a -> Transformation a
useSimpleRecognizer p = useRecognizer $ \x y -> guard (p x y) >> return []

-----------------------------------------------------------
--- Lifting

liftTrans :: View a b -> Transformation b -> Transformation a
liftTrans v = liftTransIn (v &&& identity) 

liftTransIn :: View a (b, c) -> Transformation b -> Transformation a
liftTransIn = LiftView

liftRule :: View a b -> Rule b -> Rule a
liftRule v = liftRuleIn (v &&& identity) 

liftRuleIn :: View a (b, c) -> Rule b -> Rule a
liftRuleIn v r = r
   { transformations = map (liftTransIn v) (transformations r) 
   , afterwards      = simplifyWith (mapFirst (afterwards r)) v
   }

-----------------------------------------------------------
--- QuickCheck

-- | Check the soundness of a rule: the equality function is passed explicitly
testRule :: (Arbitrary a, Show a) => (a -> a -> Bool) -> Rule a -> IO ()
testRule eq r = 
   quickCheck (propRule eq r arbitrary)

-- | Check the soundness of a rule and use a "smart generator" for this. The smart generator 
-- behaves differently on transformations constructed with a (|-), and for these transformations,
-- the left-hand side patterns are used (meta variables are instantiated with random terms)
propRuleSmart :: Show a => (a -> a -> Bool) -> Rule a -> Gen a -> Property
propRuleSmart eq r = propRule eq r . smartGen r
  
propRule :: Show a => (a -> a -> Bool) -> Rule a -> Gen a -> Property
propRule eq r gen = 
   forAll gen $ \a -> 
   forAll (smartApplyRule r a) $ \ma -> 
      isJust ma ==> (a `eq` fromJust ma)

smartGen :: Rule a -> Gen a -> Gen a
smartGen r gen = frequency [(2, gen), (1, smart)]
 where
   smart = gen >>= \a -> 
      oneof (gen : mapMaybe (smartGenTrans a) (transformations r))

smartGenTrans :: a -> Transformation a -> Maybe (Gen a)
smartGenTrans a trans =
   case trans of
      RewriteRule r _ -> return (smartGenerator r)
      LiftView v t -> do
         (b, c) <- matchM v a
         gen    <- smartGenTrans b t
         return $ liftM (\n -> build v (n, c)) gen
      _ -> Nothing

smartApplyRule :: Rule a -> a -> Gen (Maybe a)
smartApplyRule r a = do
   xss <- mapM (`smartApplyTrans` a) (transformations r)
   case concat xss of
      [] -> return Nothing
      xs -> oneof $ map (return . Just) xs

smartApplyTrans :: Transformation a -> a -> Gen [a]
smartApplyTrans trans a =
   case trans of
      Abstraction args _ g -> smartArgs args >>= \b -> smartApplyTrans (g b) a
      _ -> return (applyAll trans a)
      
smartArgs :: ArgumentList a -> Gen a
smartArgs (Single a) = genArgument a
smartArgs (Pair a b) = liftM2 (,) (smartArgs a) (smartArgs b)