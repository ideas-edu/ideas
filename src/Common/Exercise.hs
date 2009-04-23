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
-- This module defines the concept of an exercise
--
-----------------------------------------------------------------------------
module Common.Exercise 
   ( -- * Exercises
     Exercise(..), Status(..), makeExercise, stepsRemaining, getRule
{-   , identifier, domain, description, status
   , parser, subTerm, prettyPrinter
   , equivalence, equality, finalProperty
   , strategy, ruleset, differences, ordering
   , generator, suitableTerm -}
     -- * Miscellaneous
   , ExerciseCode, exerciseCode, validateCode, makeCode, readCode
   , showDerivation, showDerivations
   , checkExercise, checkParserPretty
   ) where

-- TODO: hide the Exercise constructor, and provide a default constructor
-- function that does not assume any class instances (as makeExercise currently
-- does)

import Common.Apply
import Common.Context
import Text.Parsing (Range, SyntaxError(..))
import Common.Rewriting (TreeDiff(..))
import Common.Transformation
import Common.Strategy hiding (not, fail)
import Common.Utils
import Control.Monad
import Data.Char
import Test.QuickCheck hiding (label, arguments)

data Exercise a = Exercise
   { -- identification and meta-information
     identifier    :: String    -- uniquely determines the exercise (in a given domain)
   , domain        :: String    -- e.g., math, logic, linalg
   , description   :: String    -- short sentence describing the task
   , status        :: Status
     -- parsing and pretty-printing
   , parser        :: String -> Either SyntaxError a
   , subTerm       :: String -> Range -> Maybe Location
   , prettyPrinter :: a -> String
     -- syntactic and semantic checks
   , equivalence   :: a -> a -> Bool
   , equality      :: a -> a -> Bool   -- syntactic equality
   , finalProperty :: a -> Bool
     -- strategies and rules
   , strategy      :: LabeledStrategy (Context a)
   , ruleset       :: [Rule (Context a)]
   , differences   :: a -> a -> [([Int], TreeDiff)]
   , ordering      :: a -> a -> Ordering
     -- term generation
   , generator     :: Gen a
   , suitableTerm  :: a -> Bool
   }
   
data Status = Stable | Experimental deriving (Show, Eq)

instance Apply Exercise where
   applyAll e a = map fromContext $ applyAll (strategy e) (inContext a)

-- default values for all fields
makeExercise :: (Arbitrary a, Ord a, Show a) => Exercise a
makeExercise = Exercise
   { identifier    = "<no identifier>"
   , domain        = ""
   , description   = "<no description>"
   , status        = Experimental
   , parser        = const $ Left $ ErrorMessage "No parser available"
   , subTerm       = \_ _ -> Nothing
   , prettyPrinter = show
   , equivalence   = (==)
   , equality      = (==)
   , finalProperty = const True
   , ruleset       = []
   , differences   = \_ _ -> [([], Different)]
   , ordering      = compare
   , strategy      = label "Succeed" succeed
   , generator     = arbitrary
   , suitableTerm  = const True
   }

---------------------------------------------------------------
-- Exercise codes (unique identification)

data ExerciseCode = EC String String
   deriving (Eq, Ord)

readCode :: String -> Maybe ExerciseCode
readCode xs =
   case break (=='.') xs of
      (as, _:bs) | all isAlphaNum (as++bs) -> 
         return $ EC (map toLower as) (map toLower bs)
      _ -> Nothing

makeCode :: String -> String -> ExerciseCode
makeCode d i = EC (map toLower d) (filter p (map toLower i))
 where p c = isAlphaNum c || c `elem` extraSymbols

exerciseCode :: Exercise a -> ExerciseCode
exerciseCode ex = makeCode (domain ex) (identifier ex)

validateCode :: Exercise a -> Bool
validateCode ex = all isLower (domain ex) && all p (identifier ex)
 where p c = isLower c || isDigit c || c `elem` extraSymbols

extraSymbols :: String
extraSymbols = "-."

instance Show ExerciseCode where
   show (EC xs ys) = xs ++ "." ++ ys
   
-- Temporarily. To do: replace this function by a Typed Abstract Service  
stepsRemaining :: Prefix a -> a -> Int
stepsRemaining p0 a = 
   case safeHead (runPrefix p0 a) of -- run until the end
      Nothing -> 0
      Just (_, prefix) ->
         length [ () | Step _ r <- drop (length $ prefixToSteps p0) (prefixToSteps prefix), isMajorRule r ] 
         
getRule :: Monad m => Exercise a -> String -> m (Rule (Context a))
getRule ex s = 
   case filter ((==s) . name) (ruleset ex) of 
      [hd] -> return hd
      []   -> fail $ "Could not find ruleid " ++ s
      _    -> fail $ "Ambiguous ruleid " ++ s

showDerivations :: Exercise a -> [a] -> IO ()
showDerivations ex xs = mapM_ f (zip [1..] xs)
 where
   f (i, x) = do
      putStrLn (replicate 50 '-')
      putStrLn $ "-- Exercise " ++ show i ++ "\n"
      showDerivation ex x

showDerivation :: Exercise a -> a -> IO ()
showDerivation ex start =
   putStrLn $ unlines $ 
   case derivations (unlabel (strategy ex)) (inContext start) of 
      [] -> [f (inContext start), "    => no derivation"]
      (a, xs):_ -> f a : concatMap g xs
 where
   f a = "  " ++ prettyPrinter ex (fromContext a)
   g (r, a)
      | isMinorRule r = []
      | otherwise =  ["    => " ++ show r, f a]
         
---------------------------------------------------------------
-- Checks for an exercise

-- | An instance of the Arbitrary type class is required because the random
-- | term generator that is part of an Exercise is not used for the checks:
-- | the terms produced by this generator will typically be biased.

checkExercise :: (Arbitrary a, Show a) => Exercise a -> IO ()
checkExercise = checkExerciseWith g 
 where g eq = checkRuleSmart $ \x y -> fromContext x `eq` fromContext y

checkExerciseWith :: (Arbitrary a, Show a) => ((a -> a -> Bool) -> Rule (Context a) -> IO b) -> Exercise a -> IO ()
checkExerciseWith f a = do 
   putStrLn ("** " ++ identifier a ++ " **")
   let check txt p = putLabel txt >> quickCheck p
   check "parser/pretty printer" $ 
      checkParserPretty (equivalence a) (parser a) (prettyPrinter a)
--   check "equality relation" $ 
--      checkEquivalence (ruleset a) (equality a) 
--   check "equivalence relation" $ 
--      checkEquivalence (ruleset a) (equivalence a)
--   check "equality/equivalence" $ \x -> 
--      forAll (similar (ruleset a) x) $ \y ->
--      equality a x y ==> equivalence a x y
   putStrLn "Soundness non-buggy rules" 
   flip mapM_ (filter (not . isBuggyRule) $ ruleset a) $ \r -> 
      putLabel ("    " ++ name r) >> f (equivalence a) r
   check "non-trivial terms" $ 
      forAll (generator a) $ \x -> 
      let trivial  = finalProperty a x
          rejected = not (suitableTerm a x) && not trivial
          suitable = suitableTerm a x && not trivial in
      classify trivial  "trivial"  $
      classify rejected "rejected" $
      classify suitable "suitable" $ property True 
   check "soundness strategy/generator" $ 
      forAll (generator a) $ \x -> 
      finalProperty a (fromContext $ applyD (strategy a) (inContext x))

-- check combination of parser and pretty-printer
checkParserPretty :: (a -> a -> Bool) -> (String -> Either b a) -> (a -> String) -> a -> Bool
checkParserPretty eq parser pretty p = 
   either (const False) (eq p) (parser (pretty p))

{-
checkEquivalence :: (Arbitrary a, Show a) => [Rule (Context a)] -> (a -> a -> Bool) -> a -> Property
checkEquivalence rs eq x =
   forAll (similar rs x) $ \y ->
   forAll (similar rs y) $ \z ->
      eq x x && (eq x y == eq y x) && (if eq x y && eq y z then eq x z else True) 
   
similar :: Arbitrary a => [Rule (Context a)] -> a -> Gen a
similar rs a =
   let new = a : [ fromContext cb | r <- rs, cb <- applyAll r (inContext a) ]
   in oneof [arbitrary, oneof $ map return new] -}