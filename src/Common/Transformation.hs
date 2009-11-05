{-# LANGUAGE ExistentialQuantification #-} 
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
-- This module defines transformations. Given a term, a transformation returns a list of 
-- results (often a singleton list or the empty list). A transformation can be parameterized
-- with one or more arguments. A rule is in essence just a transformation with a name (which 
-- should be unique). Both transformations and rules can be lifted to work on more complex domains. 
--
-----------------------------------------------------------------------------
module Common.Transformation 
   ( -- * Transformations
     Transformation(RewriteRule), makeTrans, makeTransList
   , getPatternPair
     -- * Arguments
   , ArgDescr(..), defaultArgDescr, Argument(..)
   , supply1, supply2, supply3, supplyLabeled1, supplyLabeled2, supplyLabeled3, supplyWith1
   , hasArguments, expectedArguments, getDescriptors, useArguments
     -- * Rules
   , Rule, name, isMinorRule, isMajorRule, isBuggyRule, isRewriteRule
   , ruleGroups, addRuleToGroup
   , rule, ruleList, ruleListF
   , makeRule, makeRuleList, makeSimpleRule, makeSimpleRuleList
   , idRule, checkRule, emptyRule, minorRule, buggyRule, doBefore, doAfter
   , transformations, getRewriteRules
     -- * Lifting
   , ruleOnce, ruleOnce2, ruleMulti, ruleMulti2, ruleSomewhere
   , liftRule, liftTrans, liftRuleIn, liftTransIn
     -- * QuickCheck
   , testRule, testRuleSmart
   ) where

import Common.Apply
import Common.Rewriting hiding (match, matchM)
import Common.Traversable
import Common.Uniplate (Uniplate, somewhereM)
import Common.Utils
import Common.View
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Ratio
import Test.QuickCheck hiding (arguments)

-----------------------------------------------------------
--- Transformations

-- | Abstract data type for representing transformations
data Transformation a
   = Function String (a -> [a])
   | RewriteRule (RewriteRule a)
   | forall b . Abstraction (ArgumentList b) (a -> Maybe b) (b -> Transformation a)
   | forall b c . LiftView (View a (b, c)) (Transformation b)
   
instance Apply Transformation where
   applyAll (Function _ f)      = f
   applyAll (RewriteRule r)     = rewriteM r
   applyAll (Abstraction _ f g) = \a -> maybe [] (\b -> applyAll (g b) a) (f a)
   applyAll (LiftView v t)      = \a -> [ build v (b, c) | (b0, c) <- matchM v a, b <- applyAll t b0  ]
   
-- | Turn a function (which returns its result in the Maybe monad) into a transformation 
makeTrans :: String -> (a -> Maybe a) -> Transformation a
makeTrans s f = makeTransList s (maybe [] return . f)

-- | Turn a function (which returns a list of results) into a transformation 
makeTransList :: String -> (a -> [a]) -> Transformation a
makeTransList = Function

getPatternPair :: a -> Transformation a -> Maybe (a, a)
getPatternPair _ (RewriteRule r) = let a :~> b = rulePair r 0 in Just (a, b)
getPatternPair a (LiftView v t) = do
   (b, c) <- match v a
   (x, y) <- getPatternPair b t
   return (build v (x, c), build v (y, c))
getPatternPair _ _ = Nothing

-----------------------------------------------------------
--- Arguments

-- | A data type for describing an argument of a parameterized transformation
data ArgDescr a = ArgDescr
   { labelArgument   :: String               -- ^ Label that is shown to the user when asked to supply the argument
   , defaultArgument :: Maybe a              -- ^ Default value that can be used
   , parseArgument   :: String -> Maybe a    -- ^ A parser 
   , showArgument    :: a -> String          -- ^ A pretty-printer
   , genArgument     :: Gen a                -- ^ An arbitrary argument generator
   }

-- | Constructor function for an argument descriptor that uses the Show and Read type classes
defaultArgDescr :: (Show a, Read a, Arbitrary a) => String -> ArgDescr a
defaultArgDescr descr = ArgDescr descr Nothing parse show arbitrary
 where 
   parse s = case reads s of
                [(a, xs)] | all isSpace xs -> return a
                _ -> Nothing

-- | A type class for types which have an argument descriptor
class Arbitrary a => Argument a where
   makeArgDescr :: String -> ArgDescr a   -- ^ The first argument is the label of the argument descriptor

instance Argument Int where
   makeArgDescr = defaultArgDescr

instance Argument Integer where
   makeArgDescr = defaultArgDescr

instance (Integral a, Arbitrary a) => Argument (Ratio a) where
   makeArgDescr = ratioArgDescr

-- | Parameterization with one argument using a default label
supply1 :: Argument x => 
             (a -> Maybe x) -> (x -> Transformation a) -> Transformation a
supply1 = supplyLabeled1 "argument 1"

-- | Parameterization with two arguments using default labels
supply2 :: (Argument x, Argument y) => 
             (a -> Maybe (x, y)) -> (x -> y -> Transformation a) -> Transformation a
supply2 = supplyLabeled2 ("argument 1", "argument 2")

-- | Parameterization with three arguments using default labels
supply3 :: (Argument x, Argument y, Argument z) => 
             (a -> Maybe (x, y, z)) -> (x -> y -> z -> Transformation a) -> Transformation a
supply3 = supplyLabeled3 ("argument 1", "argument 2", "argument 3")

-- | Parameterization with one argument using the provided label
supplyLabeled1 :: Argument x 
                  => String -> (a -> Maybe x)
                  -> (x -> Transformation a) -> Transformation a
supplyLabeled1 s f t = 
   let args = cons (makeArgDescr s) nil
       nest a = (a, ())
   in Abstraction args (fmap nest . f) (\(a, ()) -> t a)

supplyWith1 :: ArgDescr x -> (a -> Maybe x)
                  -> (x -> Transformation a) -> Transformation a
supplyWith1 descr f t = 
   let args = cons descr nil
       nest a = (a, ())
   in Abstraction args (fmap nest . f) (\(a, ()) -> t a)
   
-- | Parameterization with two arguments using the provided labels
supplyLabeled2 :: (Argument x, Argument y) 
                   => (String, String) -> (a -> Maybe (x, y)) 
                   -> (x -> y -> Transformation a) -> Transformation a
supplyLabeled2 (s1, s2) f t = 
   let args = cons (makeArgDescr s1) (cons (makeArgDescr s2) nil)
       nest (a, b) = (a, (b, ()))
   in Abstraction args (fmap nest . f) (\(a, (b, ())) -> t a b)

-- | Parameterization with three arguments using the provided labels
supplyLabeled3 :: (Argument x, Argument y, Argument z) 
                  => (String, String, String) -> (a -> Maybe (x, y, z)) 
                  -> (x -> y -> z -> Transformation a) -> Transformation a
supplyLabeled3 (s1, s2, s3) f t =
   let args = cons (makeArgDescr s1) (cons (makeArgDescr s2) (cons (makeArgDescr s3) nil))
       nest (a, b, c) = (a, (b, (c, ())))
   in Abstraction args (fmap nest . f) (\(a, (b, (c, ()))) -> t a b c)

-- | Checks whether a rule is parameterized
hasArguments :: Rule a -> Bool
hasArguments = not . null . getDescriptors

-- | Returns a list of argument descriptors
getDescriptors :: Rule a -> [Some ArgDescr]
getDescriptors rule =
   case transformations rule of
      [Abstraction args _ _] -> someArguments args
      [LiftView _ t] -> getDescriptors $ rule 
         { transformations = [t]
         , doBeforeHook    = id
         , doAfterHook     = id
         }
      _                      -> []

-- | Returns a list of pretty-printed expected arguments. Nothing indicates that there are no such arguments
expectedArguments :: Rule a -> a -> Maybe [String]
expectedArguments rule a =
   case transformations rule of
      [Abstraction args f _] -> 
         fmap (showArguments args) (f a)
      [LiftView v t] -> do 
         (b, _) <- match v a
         expectedArguments rule 
            { transformations = [t]
            , doBeforeHook    = id
            , doAfterHook     = id
            } b
      _ -> Nothing

-- | Transform a rule and use a list of pretty-printed arguments. Nothing indicates that the arguments are 
-- invalid (not parsable), or that the wrong number of arguments was supplied
useArguments :: [String] -> Rule a -> Maybe (Rule a)
useArguments list rule =
   case transformations rule of
      [t] -> do new <- make t
                return rule {transformations = [new]}
      _   -> Nothing
 where   
   make :: Transformation a -> Maybe (Transformation a)
   make trans = 
      case trans of
         Abstraction args _ g -> fmap g (parseArguments args list)
         LiftView v t         -> fmap (LiftView v) (make t)
         _                    -> Nothing
   
-----------------------------------------------------------
--- Internal machinery for arguments
               
data ArgumentList a
   = Nil a
   | forall b c . Cons ((b, c) -> a, a -> (b, c)) (ArgDescr b) (ArgumentList c)

-- smart constructor
nil :: ArgumentList ()
nil = Nil ()

-- smart constructor (provides the isomorphism proofs)
cons :: ArgDescr a -> ArgumentList b -> ArgumentList (a, b)
cons = Cons (id, id)

showArguments :: ArgumentList a -> a -> [String]
showArguments (Nil _) _ = []
showArguments (Cons (_, f) arg list) a =
   let (b, c) = f a
   in showArgument arg b : showArguments list c
   
parseArguments :: ArgumentList a -> [String] -> Maybe a
parseArguments (Nil a) [] = Just a 
parseArguments (Cons (f, _) arg list) (x:xs) = do
   b <- parseArgument  arg  x
   c <- parseArguments list xs
   return $ f (b, c)
parseArguments _ _ = Nothing
   
someArguments :: ArgumentList a -> [Some ArgDescr]
someArguments (Nil _) = []
someArguments (Cons _ arg list) = Some arg : someArguments list

ratioArgDescr :: (Integral a, Arbitrary a) => String -> ArgDescr (Ratio a)
ratioArgDescr descr = ArgDescr descr Nothing parseRatio showRatio arbitrary
 where
   showRatio  r = show (numerator r) ++ if denominator r == 1 then "" else '/' : show (denominator r)
   parseRatio s = 
      let readDivOp s = 
             case dropWhile isSpace s of
                ('/':rest) -> return rest
                [] -> return "1"
                _  -> fail "no (/) operator" 
      in safeHead [ fromInteger x / fromInteger y 
                  | (x, s1) <- reads s
                  , s2 <- readDivOp s1
                  , (y, s3) <- reads s2
                  , y /= 0
                  , all isSpace s3 
                  ]
      
-----------------------------------------------------------
--- Rules

-- | Abstract data type for representing rules
data Rule a = Rule 
   { name            :: String -- ^ Returns the name of the rule (should be unique)
   , transformations :: [Transformation a]
   , isBuggyRule     :: Bool -- ^ Inspect whether or not the rule is buggy (unsound)
   , isMinorRule     :: Bool -- ^ Returns whether or not the rule is minor (i.e., an administrative step that is automatically performed by the system)
   , ruleGroups      :: [String]
   , doBeforeHook    :: a -> a -- ^ Hook to perform an action before the rule is fired
   , doAfterHook     :: a -> a -- ^ Hook to perform an action after the rule has been fired
   }

instance Show (Rule a) where
   show = name

instance Eq (Rule a) where
   r1 == r2 = name r1 == name r2

instance Apply Rule where
   applyAll r a = do 
      let b = doBeforeHook r a
      t <- transformations r
      c <- applyAll t b
      return (doAfterHook r c)

-- | Returns whether or not the rule is major (i.e., not minor)
isMajorRule :: Rule a -> Bool
isMajorRule = not . isMinorRule

isRewriteRule :: Rule a -> Bool
isRewriteRule = all p . transformations
 where
   p :: Transformation a -> Bool
   p (RewriteRule _) = True
   p (LiftView _ t)  = p t
   p _               = False

addRuleToGroup :: String -> Rule a -> Rule a
addRuleToGroup group r = r { ruleGroups = group : ruleGroups r }

ruleList :: Builder f a => String -> [f] -> Rule a
ruleList s = makeRuleList s . map (RewriteRule . rewriteRule s)

ruleListF :: BuilderList f a => String -> f -> Rule a
ruleListF s = makeRuleList s . map RewriteRule . rewriteRules s

rule :: Builder f a => String -> f -> Rule a
rule s = makeRule s . RewriteRule . rewriteRule s

-- | Turn a transformation into a rule: the first argument is the rule's name
makeRule :: String -> Transformation a -> Rule a
makeRule n = makeRuleList n . return

-- | Turn a list of transformations into a single rule: the first argument is the rule's name
makeRuleList :: String -> [Transformation a] -> Rule a
makeRuleList n ts = Rule n ts False False [] id id

-- | Turn a function (which returns its result in the Maybe monad) into a rule: the first argument is the rule's name
makeSimpleRule :: String -> (a -> Maybe a) -> Rule a
makeSimpleRule n = makeRule n . makeTrans n

-- | Turn a function (which returns a list of results) into a rule: the first argument is the rule's name
makeSimpleRuleList :: String -> (a -> [a]) -> Rule a
makeSimpleRuleList n = makeRule n . makeTransList n

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

-- | Perform the function before the rule has been fired
doBefore :: (a -> a) -> Rule a -> Rule a
doBefore f r = r { doBeforeHook = f }

-- | Perform the function after the rule has been fired
doAfter :: (a -> a) -> Rule a -> Rule a
doAfter f r = r { doAfterHook = f }

getRewriteRules :: Rule a -> [(Some RewriteRule, Bool)]
getRewriteRules r = concatMap f (transformations r)
 where
   f :: Transformation a -> [(Some RewriteRule, Bool)]
   f trans =
      case trans of
         RewriteRule rr -> [(Some rr, not $ isBuggyRule r)]      
         LiftView _ t   -> f t
         _              -> []

-----------------------------------------------------------
--- Lifting

-- | Lift a rule using the Once type class
ruleOnce :: Once f => Rule a -> Rule (f a)
ruleOnce r = makeSimpleRuleList (name r) $ onceM $ applyAll r

-- | Apply a rule once (in two functors)
ruleOnce2 :: (Once f, Once g) => Rule a -> Rule (f (g a))
ruleOnce2 = ruleOnce . ruleOnce

-- | Apply at multiple locations, but at least once
ruleMulti :: (Switch f, Crush f) => Rule a -> Rule (f a)
ruleMulti r = makeSimpleRuleList (name r) $ multi $ applyAll r

-- | Apply at multiple locations, but at least once (in two functors)
ruleMulti2 :: (Switch f, Crush f, Switch g, Crush g) => Rule a -> Rule (f (g a))
ruleMulti2 = ruleMulti . ruleMulti

multi :: (Switch f, Crush f) => (a -> [a]) -> f a -> [f a]
multi f a =
   let g a = case f a of 
                [] -> [(False, a)]
                xs -> zip (repeat True) xs
       xs = switch (fmap g a)
       p = any fst . crush
   in map (fmap snd) (filter p xs)

ruleSomewhere :: Uniplate a => Rule a -> Rule a
ruleSomewhere r = makeSimpleRuleList (name r) $ somewhereM $ applyAll r

liftTrans :: View a b -> Transformation b -> Transformation a
liftTrans v = liftTransIn (v &&& identity) 

liftTransIn :: View a (b, c) -> Transformation b -> Transformation a
liftTransIn = LiftView

liftRule :: View a b -> Rule b -> Rule a
liftRule v = liftRuleIn (v &&& identity) 

liftRuleIn :: View a (b, c) -> Rule b -> Rule a
liftRuleIn v r = r
   { transformations = map (liftTransIn v) (transformations r)
   , doBeforeHook    = liftFun (doBeforeHook r)
   , doAfterHook     = liftFun (doAfterHook r)
   }
 where
   liftFun f = simplifyWith (first f) v
      
-----------------------------------------------------------
--- QuickCheck

-- | Check the soundness of a rule: the equality function is passed explicitly
testRule :: (Arbitrary a, Show a) => (a -> a -> Bool) -> Rule a -> IO ()
testRule eq rule = 
   quickCheck (propRule eq rule arbitrary)

-- | Check the soundness of a rule and use a "smart generator" for this. The smart generator 
-- behaves differently on transformations constructed with a (|-), and for these transformations,
-- the left-hand side patterns are used (meta variables are instantiated with random terms)
testRuleSmart :: Show a => (a -> a -> Bool) -> Rule a -> Gen a -> IO ()
testRuleSmart eq rule gen =
   quickCheck (propRule eq rule (smartGen rule gen))
  
propRule :: Show a => (a -> a -> Bool) -> Rule a -> Gen a -> Property
propRule eq rule gen = 
   forAll gen $ \a -> 
   forAll (smartApplyRule rule a) $ \ma -> 
      isJust ma ==> (a `eq` fromJust ma)

smartGen :: Rule a -> Gen a -> Gen a
smartGen r gen = frequency [(2, gen), (1, smart)]
 where
   smart = gen >>= \a -> 
      oneof (gen : mapMaybe (smartGenTrans a) (transformations r))

smartGenTrans :: a -> Transformation a -> Maybe (Gen a)
smartGenTrans a trans =
   case trans of
      RewriteRule r -> return (smartGenerator r)
      LiftView v t -> do
         (b, c) <- match v a
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
smartArgs (Nil a) = return a
smartArgs (Cons (f, _) descr xs) = liftM2 (curry f) (genArgument descr) (smartArgs xs)