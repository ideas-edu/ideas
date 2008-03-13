-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- This module defines the concept of an exercise, and services that operate
-- one these exercises.
--
-----------------------------------------------------------------------------
module Common.Exercise 
   ( -- * Exercises
     Exercise(..), makeExercise
     -- * Services
   , Feedback(..), giveStep, giveSteps, giveHint, feedback, randomTerm, stepsRemaining
     -- * Formatted documents
   , Doc, text, showDoc
     -- * QuickCheck utilities
   , checkExercise, checkParserPretty
   ) where

import Common.Apply
import Common.Context (Location)
import Common.Parsing (Range)
import Common.Transformation
import Common.Strategy hiding (not)
import Common.Utils
import Common.Unification
import Control.Monad
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import System.Random
import Test.QuickCheck hiding (label, arguments)

data Exercise a = Exercise
   { shortTitle    :: String
   , parser        :: String -> Either (Doc a) a
   , subTerm       :: String -> Range -> Maybe Location
   , prettyPrinter :: a -> String
   , equivalence   :: a -> a -> Bool
   , equality      :: a -> a -> Bool -- syntactic equality
   , finalProperty :: a -> Bool
   , ruleset       :: [Rule a]
   , strategy      :: LabeledStrategy a
   , generator     :: Gen a
   , suitableTerm  :: a -> Bool
   }

instance Apply Exercise where
   applyAll = applyAll . strategy

-- default values for all fields
makeExercise :: (Arbitrary a, Eq a, Show a) => Exercise a
makeExercise = Exercise
   { shortTitle    = "no short title"
   , parser        = const $ Left (text "no parser")
   , subTerm       = \_ _ -> Nothing
   , prettyPrinter = show
   , equivalence   = (==)
   , equality      = (==)
   , finalProperty = const True
   , ruleset       = []
   , strategy      = label "Succeed" succeed
   , generator     = arbitrary
   , suitableTerm  = const True
   }
   
randomTerm :: Exercise a -> IO a
randomTerm a = do 
   stdgen <- newStdGen
   return (randomTermWith stdgen a)

-- | Default size is 100
randomTermWith :: StdGen -> Exercise a -> a
randomTermWith stdgen a
   | not (suitableTerm a term) =
        randomTermWith (snd $ next stdgen) a
   | otherwise =
        term
 where
   term = generate 100 stdgen (generator a)

-- | Returns a text and the rule that is applicable
giveHint :: Prefix a -> a -> Maybe (Doc a, Rule a)
giveHint p = safeHead . giveHints p

-- | Returns a text and the rule that is applicable
giveHints :: Prefix a -> a -> [(Doc a, Rule a)]
giveHints p = map g . giveSteps p
 where
   g (x, y, _, _, _) = (x, y)

giveStep :: Prefix a -> a -> Maybe (Doc a, Rule a, Prefix a, a, a)
giveStep p = safeHead . giveSteps p

giveSteps :: Prefix a -> a -> [(Doc a, Rule a, Prefix a, a, a)]
giveSteps p0 a = 
   let make (new, prefix) = 
          let steps  = prefixToSteps prefix
              minors = stepsToRules (drop (length $ prefixToSteps p0) steps)
              old    = if null minors then a else applyListD (init minors) a
          in case lastRuleInPrefix prefix of
                Just r -> [ (doc r old, r, prefix, old, new) | isMajorRule r ]
                _      -> []
       showList xs = "(" ++ concat (intersperse "," xs) ++ ")"
       doc r old = text "Use rule " <> rule r <> 
          case expectedArguments r old of
             Just xs -> text "\n   with arguments " <> text (showList xs)
             Nothing -> emptyDoc
   in concatMap make $ runPrefixMajor p0 a            
         
feedback :: Exercise a -> Prefix a -> a -> String -> Feedback a
feedback ex p0 a txt =
   case parser ex txt of
      Left msg -> 
         SyntaxError msg
      Right new
         | not (equivalence ex a new) -> 
              Incorrect (text "Incorrect")
         | otherwise -> 
              let answers = giveSteps p0 a
                  check (_, _, _, _, this) = equality ex new this
              in case filter check answers of
                    (_, r, newPrefix, _, newTerm):_ -> Correct (text "Well done! You applied rule " <> rule r) (Just (newPrefix, r, newTerm))
                    _ | equality ex a new -> 
                         Correct (text "You have submitted the current term.") Nothing
                    _ -> Correct (text "Equivalent, but not a known rule. Please retry.") Nothing

stepsRemaining :: Prefix a -> a -> Int
stepsRemaining p0 a = 
   case safeHead (runPrefixLocation [] p0 a) of -- run until the end
      Nothing -> 0
      Just (_, prefix) ->
         length [ () | Step _ r <- drop (length $ prefixToSteps p0) (prefixToSteps prefix), isMajorRule r ] 

data Feedback a = SyntaxError (Doc a)
                | Incorrect   (Doc a)
                | Correct     (Doc a) (Maybe (Prefix a, Rule a, a)) {- The rule that was applied -}

getRuleNames :: Exercise a -> [String]
getRuleNames = map name . ruleset

---------------------------------------------------------------
-- Documents (feedback with structure)
                
newtype Doc a = D [DocItem a]

data DocItem a = Text String | Term a | DocRule (Some Rule)

instance Functor Doc where
   fmap f (D xs) = D (map (fmap f) xs)

instance Functor DocItem where
   fmap f (Text s) = Text s
   fmap f (Term a) = Term (f a)
   fmap f (DocRule r) = DocRule r 

emptyDoc :: Doc a
emptyDoc = D []

showDoc :: Exercise a -> Doc a -> String
showDoc = showDocWith . prettyPrinter

showDocWith :: (a -> String) -> Doc a -> String
showDocWith f (D xs) = concatMap g xs
 where
   g (Text s) = s
   g (Term a) = f a 
   g (DocRule (Some r)) = name r
   
infixr 5 <>

(<>) :: Doc a -> Doc a -> Doc a
D xs <> D ys = D (xs ++ ys)

docs :: [Doc a] -> Doc a
docs = foldr (<>) emptyDoc

text :: String -> Doc a
text s = D [Text s]

term :: a -> Doc a
term a = D [Term a]

rule :: Rule a -> Doc a
rule r = D [DocRule (Some r)]

---------------------------------------------------------------
-- Checks for an exercise

-- | An instance of the Arbitrary type class is required because the random
-- | term generator that is part of an Exercise is not used for the checks:
-- | the terms produced by this generator will typically be biased.

checkExercise :: (Arbitrary a, Show a) => Exercise a -> IO ()
checkExercise = checkExerciseWith checkRule

checkExerciseSmart :: (Arbitrary a, Show a, Substitutable a) => Exercise a -> IO ()
checkExerciseSmart = checkExerciseWith checkRuleSmart

checkExerciseWith :: (Arbitrary a, Show a) => ((a -> a -> Bool) -> Rule a -> IO b) -> Exercise a -> IO ()
checkExerciseWith f a = do
   putStrLn ("Checking exercise: " ++ shortTitle a)
   let check txt p = putStr ("- " ++ txt ++ "\n    ") >> quickCheck p
   check "parser/pretty printer" $ 
      checkParserPretty (equivalence a) (parser a) (prettyPrinter a)
   check "equality relation" $ 
      checkEquivalence (ruleset a) (equality a)
   check "equivalence relation" $ 
      checkEquivalence (ruleset a) (equivalence a)
   check "equality/equivalence" $ \x -> 
      forAll (similar (ruleset a) x) $ \y ->
      equality a x y ==> equivalence a x y
   putStrLn "- Soundness non-buggy rules"
   flip mapM_ (filter (not . isBuggyRule) $ ruleset a) $ \r -> 
      putStr "    " >> f (equivalence a) r
   check "non-trivial terms" $ 
      forAll (sized $ \_ -> generator a) $ \x -> 
      let trivial  = finalProperty a x
          rejected = not (suitableTerm a x) && not trivial
          suitable = suitableTerm a x && not trivial in
      classify trivial  "trivial"  $
      classify rejected "rejected" $
      classify suitable "suitable" $ property True
   check "soundness strategy/generator" $ 
      forAll (generator a) $ \x -> 
      finalProperty a (applyD (strategy a) x)
      

-- check combination of parser and pretty-printer
checkParserPretty :: (a -> a -> Bool) -> (String -> Either b a) -> (a -> String) -> a -> Bool
checkParserPretty eq parser pretty p = 
   either (const False) (eq p) (parser (pretty p))
   
checkEquivalence :: (Arbitrary a, Show a) => [Rule a] -> (a -> a -> Bool) -> a -> Property
checkEquivalence rs eq x = 
   forAll (similar rs x) $ \y ->
   forAll (similar rs y) $ \z ->
      eq x x && (eq x y == eq y x) && (if eq x y && eq y z then eq x z else True)
   
similar :: Arbitrary a => [Rule a] -> a -> Gen a
similar rs a = 
   let new = a : concatMap (\r -> applyAll r a) rs
   in oneof [arbitrary, oneof $ map return new]