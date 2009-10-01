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
-- This module defines the concept of an exercise
--
-----------------------------------------------------------------------------
module Common.Exercise 
   ( -- * Exercises
     Exercise, Status(..), makeExercise, emptyExercise
   , description, exerciseCode, status, parser, prettyPrinter
   , equivalence, equality, finalProperty, strategy, ruleset, differences
   , ordering, termGenerator
   , stepsRemaining, getRule
   , TermGenerator(..), makeGenerator, simpleGenerator, randomTerm, randomTermWith
     -- * Miscellaneous
   , ExerciseCode, domain, identifier, makeCode, readCode
   , showDerivation, showDerivationWith, showDerivations, printDerivation, printDerivations
   , checkExercise, checkParserPretty
   , checksForList
   ) where

import Common.Apply
import Common.Context
import Common.Rewriting (TreeDiff(..))
import Common.Strategy hiding (not, fail, replicate)
import Common.Transformation
import Common.Utils
import Control.Monad.Error
import Data.Char
import System.Random
import Test.QuickCheck hiding (label, arguments)
import Text.Parsing (SyntaxError(..))

data Exercise a = Exercise
   { -- identification and meta-information
     description   :: String       -- short sentence describing the task
   , exerciseCode  :: ExerciseCode -- uniquely determines the exercise (in a given domain)
   , status        :: Status
     -- parsing and pretty-printing
   , parser        :: String -> Either SyntaxError a
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
   , termGenerator :: TermGenerator a
   }
   
data Status = Stable | Provisional | Experimental deriving (Show, Eq)

instance Apply Exercise where
   applyAll e = map fromContext . applyAll (strategy e) . inContext

-- default values for all fields
makeExercise :: (Arbitrary a, Ord a, Show a) => Exercise a
makeExercise = Exercise
   { description   = "<no description>"
   , exerciseCode  = error "no exercise code"
   , status        = Experimental
   , parser        = const $ Left $ ErrorMessage "No parser available"
   , prettyPrinter = show
   , equivalence   = (==)
   , equality      = (==)
   , finalProperty = const True
   , ruleset       = []
   , differences   = \_ _ -> [([], Different)]
   , ordering      = compare
   , strategy      = label "Succeed" succeed
   , termGenerator = simpleGenerator arbitrary
   }
   
emptyExercise :: Exercise a
emptyExercise = Exercise {}

---------------------------------------------------------------
-- Exercise generators

data TermGenerator a 
   = ExerciseList [a] 
   | ExerciseGenerator (a -> Bool) (Gen a)

simpleGenerator :: Gen a -> TermGenerator a
simpleGenerator = makeGenerator (const True)

makeGenerator :: (a -> Bool) -> Gen a -> TermGenerator a
makeGenerator = ExerciseGenerator

randomTerm :: Exercise a -> IO a
randomTerm ex = do
   rng <- newStdGen
   return (randomTermWith rng ex)

randomTermWith :: StdGen -> Exercise a -> a
randomTermWith rng ex = 
   case termGenerator ex of
      ExerciseList xs
         | null xs   -> error "randomTermWith: empty exercise list"
         | otherwise -> xs !! fst (randomR (0, length xs - 1) rng)
      ExerciseGenerator p m 
         | p a       -> a 
         | otherwise -> randomTermWith (snd $ next rng) ex
       where
         a = generate 100 rng m

---------------------------------------------------------------
-- Exercise codes (unique identification)

data ExerciseCode = EC {domain :: String, identifier :: String}
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

{-
validateCode :: Exercise a -> Bool
validateCode ex = all isLower (domain ex) && all p (identifier ex)
 where p c = isLower c || isDigit c || c `elem` extraSymbols
-}

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

showDerivations :: Exercise a -> [a] -> String
showDerivations ex xs = unlines (zipWith f [1..] xs)
 where
   f i x = unlines
      [ replicate 50 '-'
      , "-- Exercise " ++ show i ++ "\n"
      , showDerivation ex x
      ]

showDerivation :: Exercise a -> a -> String
showDerivation ex = showDerivationWith (prettyPrinter ex) (unlabel (strategy ex))

showDerivationWith :: (a -> String) -> Strategy (Context a) -> a -> String
showDerivationWith showf s start = unlines $ 
   case derivations s (inContext start) of 
      [] -> [f (inContext start), "    => no derivation"]
      (a, xs):_ -> f a : concatMap g xs
 where
   f a = "  " ++ showf (fromContext a)
   g (r, a)
      | isMinorRule r = []
      | otherwise =  ["    => " ++ show r, f a]
         
printDerivations :: Exercise a -> [a] -> IO ()
printDerivations ex = putStrLn . showDerivations ex

printDerivation :: Exercise a -> a -> IO ()
printDerivation ex = putStrLn . showDerivation ex
         
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
   putStrLn ("** " ++ show (exerciseCode a))
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
   forM (filter (not . isBuggyRule) $ ruleset a) $ \r -> 
      putLabel ("    " ++ name r) >> f (equivalence a) r
   
   case termGenerator a of 
      ExerciseList _ -> return ()
      ExerciseGenerator p m -> do
         check "non-trivial terms" $ 
            forAll m $ \x -> 
            let trivial  = finalProperty a x
                rejected = not (p x) && not trivial
                suitable = p x && not trivial in
            classify trivial  "trivial"  $
            classify rejected "rejected" $
            classify suitable "suitable" $ property True 
         check "soundness strategy/generator" $ 
            forAll m $
               finalProperty a . fromContext . applyD (strategy a) . inContext

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

checksForList :: Exercise a -> IO ()
checksForList ex =
   case termGenerator ex of
      ExerciseList xs | status ex /= Experimental -> do
         let err s = putStrLn $ "Error: " ++ s
         putStrLn ("** " ++ show (exerciseCode ex))
         mapM_ (either err return . checksForTerm ex) xs
      _ -> return ()

checksForTerm :: Monad m => Exercise a -> a -> m ()
checksForTerm ex a = 
   let txt = prettyPrinter ex a in
   case derivations (unlabel $ strategy ex) (inContext a) of
      [] -> fail $ "no derivation for " ++ txt
      (_, xs):_ -> do
         unless (finalProperty ex (last as)) $
            fail $ "not solved: " ++ txt
         case [ (x, y) | x <- as, y <- as, not (equivalence ex x y) ] of
            (x, y):_ -> fail $ "not equivalent: " ++ prettyPrinter ex x ++ "  and  "
                                                  ++ prettyPrinter ex y
            _        -> return ()
         case filter (not . checkParserPretty (equality ex) (parser ex) (prettyPrinter ex)) as of
            hd:_ -> let s = prettyPrinter ex hd in
                    fail $ "parse error for " ++ s ++ ": parsed as " ++
                           either show (prettyPrinter ex) (parser ex s)
            _    -> return ()
       where
         as = a : map (fromContext . snd) xs