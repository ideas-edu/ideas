{-# OPTIONS -XExistentialQuantification #-} 
-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
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
     Transformation(RewriteRule), makeTrans, inverseTrans, getPatternPair
     -- * Arguments
   , ArgDescr(..), defaultArgDescr, Argument(..)
   , supply1, supply2, supply3, supplyLabeled1, supplyLabeled2, supplyLabeled3
   , hasArguments, expectedArguments, getDescriptors, useArguments
     -- * Rules
   , Rule, name, isMinorRule, isMajorRule, isBuggyRule, hasInverse, isRewriteRule
   , rule, ruleList, ruleListF, makeRule, makeRuleList, makeSimpleRule, makeSimpleRuleList
   , idRule, emptyRule, minorRule, buggyRule, inverseRule, transformations, getRewriteRules
     -- * Lifting
   , LiftPair, liftPairGet, liftPairSet, liftPairChange, makeLiftPair, Lift(..)
     -- * QuickCheck
   , checkRule, checkRuleSmart
   ) where

import Data.Char
import Data.Ratio
import Data.List
import Data.Maybe
import Test.QuickCheck hiding (arguments)
import Common.Apply
import Common.Utils
import Control.Monad
import Common.Rewriting

-----------------------------------------------------------
--- Transformations

-- | Abstract data type for representing transformations
data Transformation a
   = Function String (a -> [a])
   | RewriteRule (RewriteRule a)
   | forall b . Abstraction (ArgumentList b) (a -> Maybe b) (b -> Transformation a)
   | forall b . Lift (LiftPair b a) (Transformation b)
   
instance Apply Transformation where
   applyAll (Function _ f)      = f
   applyAll (RewriteRule r)     = rewriteM r
   applyAll (Abstraction _ f g) = \a -> maybe [] (\b -> applyAll (g b) a) (f a)
   applyAll (Lift lp t )        = \b -> maybe [] (map (\new -> liftPairSet lp new b) . applyAll t) (liftPairGet lp b)
   
-- | Turn a function (which returns its result in the Maybe monad) into a transformation 
makeTrans :: String -> (a -> Maybe a) -> Transformation a
makeTrans s f = makeTransList s (maybe [] return . f)

-- | Turn a function (which returns a list of results) into a transformation 
makeTransList :: String -> (a -> [a]) -> Transformation a
makeTransList = Function

-- | Return the inverse of a transformation. Only transformation that are constructed with (|-) 
-- can be inversed
inverseTrans :: Transformation a -> Maybe (Transformation a)
inverseTrans trans = 
   case trans of
      RewriteRule r -> fmap RewriteRule (inverse r)
      Lift lp t     -> fmap (Lift lp) (inverseTrans t)
      _ -> Nothing

getPatternPair :: a -> Transformation a -> Maybe (a, a)
getPatternPair _ (RewriteRule r) = let a :~> b = rulePair r 0 in Just (a, b)
getPatternPair a (Lift lp t) = do
   let f t = liftPairSet lp t a
   b      <- liftPairGet lp a
   (x, y) <- getPatternPair b t
   return (f x, f y)
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
   
instance (Integral a, Arbitrary a) => Arbitrary (Ratio a) where
   arbitrary = liftM2 (\x y -> fromInteger x / fromInteger y) arbitrary (oneof $ map return [1..5])
   coarbitrary r = coarbitrary (numerator r) . coarbitrary (denominator r)

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
      [Lift _ t]             -> getDescriptors (rule {transformations = [t], getInverse = Nothing})
      _                      -> []

-- | Returns a list of pretty-printed expected arguments. Nothing indicates that there are no such arguments
expectedArguments :: Rule a -> a -> Maybe [String]
expectedArguments rule a =
   case transformations rule of
      [Abstraction args f _] -> fmap (showArguments args) (f a)
      [Lift lp t]            -> do b <- liftPairGet lp a
                                   expectedArguments (rule {transformations = [t], getInverse = Nothing}) b
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
         Lift lp t            -> fmap (Lift lp) (make t)     
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
cons arg list = Cons (id, id) arg list

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
   showRatio  r = show (numerator r) ++ if denominator r == 1 then "" else "/" ++ show (denominator r)
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
   , getInverse      :: Maybe (Rule a)
   }

instance Show (Rule a) where
   show = name

instance Eq (Rule a) where
   r1 == r2 = name r1 == name r2

instance Apply Rule where
   applyAll r a = concatMap (`applyAll` a) (transformations r)

-- | Returns whether or not the rule is major (i.e., not minor)
isMajorRule :: Rule a -> Bool
isMajorRule = not . isMinorRule

isRewriteRule :: Rule a -> Bool
isRewriteRule = all p . transformations
 where
   p :: Transformation a -> Bool
   p (RewriteRule _) = True
   p (Lift _ t)      = p t
   p _               = False

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
makeRuleList n ts = Rule n ts False False Nothing

-- | Turn a function (which returns its result in the Maybe monad) into a rule: the first argument is the rule's name
makeSimpleRule :: String -> (a -> Maybe a) -> Rule a
makeSimpleRule n = makeRule n . makeTrans n

-- | Turn a function (which returns a list of results) into a rule: the first argument is the rule's name
makeSimpleRuleList :: String -> (a -> [a]) -> Rule a
makeSimpleRuleList n = makeRule n . makeTransList n

-- | A special (minor) rule that always returns the identity
idRule :: Rule a
idRule = minorRule $ makeSimpleRule "Identity" return

-- | A special (minor) rule that is never applicable (i.e., this rule always fails)
emptyRule :: Rule a
emptyRule = minorRule $ makeSimpleRule "Empty" (const Nothing)

-- | Mark the rule as minor (by default, rules are not minor)
minorRule :: Rule a -> Rule a 
minorRule r = r {isMinorRule = True}

-- | Mark the rule as buggy (by default, rules are supposed to be sound)
buggyRule :: Rule a -> Rule a 
buggyRule r = r {isBuggyRule = True}

hasInverse :: Rule a -> Rule a -> Rule a
hasInverse inv r = r {getInverse = Just inv}

-- | Return the inverse of a transformation. Only rewrite rules can be inversed
inverseRule :: Rule a -> Maybe (Rule a)
inverseRule r =
   case getInverse r of
      Just new -> Just new
      Nothing  -> do
         ts <- mapM inverseTrans (transformations r)
         return r
            { name = name r ++ " [inverse]"
            , transformations = ts
            }

getRewriteRules :: Rule a -> [(Some RewriteRule, Bool)]
getRewriteRules r = concatMap f (transformations r)
 where
   f :: Transformation a -> [(Some RewriteRule, Bool)]
   f trans =
      case trans of
         RewriteRule rr -> [(Some rr, not $ isBuggyRule r)]      
         Lift _ t       -> f t
         _              -> []

-----------------------------------------------------------
--- Lifting

-- | A lift pair consists of two functions: the first to access a value in a context (this can fail,
-- hence the Maybe), the second to update the value in its context
data LiftPair a b = LiftPair 
   { liftPairGet :: b -> Maybe a -- ^ Returns the accessor function of a lift pair
   , liftPairSet :: a -> b -> b  -- ^ Returns the update function of a lift pair
   }
-- | Update a value in a context
liftPairChange :: LiftPair a b -> (a -> Maybe a) -> b -> Maybe b
liftPairChange lp f b = do 
   a   <- liftPairGet lp b
   new <- f a
   return (liftPairSet lp new b)

-- | Constructor for a lift pair
makeLiftPair :: (b -> Maybe a) -> (a -> b -> b) -> LiftPair a b
makeLiftPair = LiftPair

-- | A type class for functors that can be lifted with a lift pair
class Lift f where
   lift :: LiftPair a b -> f a -> f b

instance Lift Transformation where
   lift = Lift
   
instance Lift Rule where
   lift lp r = r { transformations = map (lift lp) (transformations r)
                 , getInverse = fmap (lift lp) (getInverse r)
                 }

-----------------------------------------------------------
--- QuickCheck

-- | Check the soundness of a rule: the equality function is passed explicitly
checkRule :: (Arbitrary a, Show a) => (a -> a -> Bool) -> Rule a -> IO ()
checkRule eq rule =
   quickCheck (propRule arbitrary eq rule)

-- | Check the soundness of a rule and use a "smart generator" for this. The smart generator 
-- behaves differently on transformations constructed with a (|-), and for these transformations,
-- the left-hand side patterns are used (meta variables are instantiated with random terms)
checkRuleSmart :: (Arbitrary a, Show a) => (a -> a -> Bool) -> Rule a -> IO ()
checkRuleSmart eq rule =
   quickCheck (propRule (smartGen rule) eq rule)
  
propRule :: (Arbitrary a, Show a) => Gen a -> (a -> a -> Bool) -> Rule a -> (a -> Bool) -> Property
propRule gen eq rule _ = 
   forAll gen $ \a -> 
   forAll (smartApplyRule rule a) $ \ma -> 
      isJust ma ==> (a `eq` fromJust ma)

smartGen :: Arbitrary a => Rule a -> Gen a
smartGen r = frequency [(4, arbitrary), (1, smartGenRule r)]

smartGenRule :: Arbitrary a => Rule a -> Gen a
smartGenRule r = do
   a <- arbitrary
   case catMaybes $ map (smartGenTrans a) (transformations r) of
      [] -> arbitrary
      gs -> oneof gs

smartGenTrans :: a -> Transformation a -> Maybe (Gen a)
smartGenTrans a trans =
   case trans of
      RewriteRule r -> return (smartGenerator r)
      Lift lp t -> do 
         b   <- liftPairGet lp a
         gen <- smartGenTrans b t
         return $ liftM (\c -> liftPairSet lp c a) gen
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