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
     Exercise, Status(..), testableExercise, makeExercise, emptyExercise
   , description, exerciseCode, status, parser, prettyPrinter
   , equivalence, similarity, isReady, isSuitable, eqWithContext
   , strategy, navigation, canBeRestarted, extraRules
   , difference, ordering, testGenerator, randomExercise, examples, getRule
   , simpleGenerator, useGenerator
   , randomTerm, randomTermWith, ruleset
   , makeContext, inContext
     -- * Exercise codes
   , ExerciseCode, noCode, makeCode, readCode, domain, identifier
     -- * Miscellaneous
   , equivalenceContext, restrictGenerator
   , showDerivation, printDerivation
   , checkExercise, checkParserPretty
   , checksForList
   ) where

import Common.Apply
import Common.Context
import Common.Strategy hiding (not, fail, replicate)
import qualified Common.Strategy as S
import Common.Derivation
import Common.Navigator
import Common.Transformation
import Common.Utils
import Control.Monad.Error
import Data.Char
import Data.List
import Data.Maybe
import System.Random
import Test.QuickCheck hiding (label, arguments)
import Text.Parsing (SyntaxError(..))

data Exercise a = Exercise
   { -- identification and meta-information
     description    :: String       -- short sentence describing the task
   , exerciseCode   :: ExerciseCode -- uniquely determines the exercise (in a given domain)
   , status         :: Status
     -- parsing and pretty-printing
   , parser         :: String -> Either SyntaxError a
   , prettyPrinter  :: a -> String
     -- syntactic and semantic checks
   , equivalence    :: a -> a -> Bool
   , similarity     :: a -> a -> Bool      -- possibly more liberal than syntactic equality
   , ordering       :: a -> a -> Ordering  -- syntactic comparison
   , isReady        :: a -> Bool
   , isSuitable     :: a -> Bool
   , difference     :: Bool -> a -> a -> Maybe (a, a)
   , eqWithContext  :: Maybe (Context a -> Context a -> Bool) -- special equivalence with context info
     -- strategies and rules
   , strategy       :: LabeledStrategy (Context a)
   , navigation     :: a -> Navigator a
   , canBeRestarted :: Bool                -- By default, assumed to be the case
   , extraRules     :: [Rule (Context a)]  -- Extra rules (possibly buggy) not appearing in strategy
     -- testing and exercise generation
   , testGenerator  :: Maybe (Gen a)
   , randomExercise :: Maybe (StdGen -> Int -> a)
   , examples       :: [a]
   }
   
data Status = Stable | Provisional | Experimental deriving (Show, Eq)

instance Eq (Exercise a) where
   e1 == e2 = exerciseCode e1 == exerciseCode e2

instance Ord (Exercise a) where
   e1 `compare` e2 = exerciseCode e1 `compare` exerciseCode e2

instance Apply Exercise where
   applyAll ex = concatMap fromContext . applyAll (strategy ex) . inContext ex

testableExercise :: (Arbitrary a, Show a, Ord a) => Exercise a
testableExercise = makeExercise
   { testGenerator = Just arbitrary
   }

makeExercise :: (Show a, Ord a) => Exercise a
makeExercise = emptyExercise
   { prettyPrinter = show
   , similarity    = (==)
   , ordering      = compare
   }
   
emptyExercise :: Exercise a
emptyExercise = Exercise 
   { -- identification and meta-information
     description    = "<<description>>" 
   , exerciseCode   = noCode
   , status         = Experimental
     -- parsing and pretty-printing
   , parser         = const $ Left $ ErrorMessage "<<no parser>>"
   , prettyPrinter  = const "<<no pretty-printer>>"
     -- syntactic and semantic checks
   , equivalence    = \_ _ -> True
   , similarity     = \_ _ -> True
   , ordering       = \_ _ -> EQ
   , isReady        = const True
   , isSuitable     = const True
   , difference     = \_ _ _ -> Nothing
   , eqWithContext  = Nothing
     -- strategies and rules
   , strategy       = label "Fail" S.fail
   , navigation     = noNavigator
   , canBeRestarted = True
   , extraRules     = [] 
     -- testing and exercise generation
   , testGenerator  = Nothing
   , randomExercise = Nothing
   , examples       = []
   }
   
makeContext :: Exercise a -> Environment -> a -> Context a
makeContext ex env = newContext env . navigation ex

-- | Put a value into an empty environment
inContext :: Exercise a -> a -> Context a
inContext = flip makeContext emptyEnv

---------------------------------------------------------------
-- Exercise generators

-- returns a sorted list of rules (no duplicates)
ruleset :: Exercise a -> [Rule (Context a)]
ruleset ex = nub (sortBy cmp list)
 where 
   list = rulesInStrategy (strategy ex) ++ extraRules ex
   cmp a b = name a `compare` name b
 
simpleGenerator :: Gen a -> Maybe (StdGen -> Int -> a) 
simpleGenerator = useGenerator (const True) . const

useGenerator :: (a -> Bool) -> (Int -> Gen a) -> Maybe (StdGen -> Int -> a) 
useGenerator p g = Just f
 where
   f rng level 
      | p a       = a
      | otherwise = f (snd (next rng)) level
    where
      a = generate 100 rng (g level)

restrictGenerator :: (a -> Bool) -> Gen a -> Gen a
restrictGenerator p g = do
   a <- g 
   if p a then return a 
          else restrictGenerator p g

randomTerm :: Int -> Exercise a -> IO a
randomTerm level ex = do
   rng <- newStdGen
   return (randomTermWith rng level ex)

randomTermWith :: StdGen -> Int -> Exercise a -> a
randomTermWith rng level ex = 
   case randomExercise ex of
      Just f  -> f rng level
      Nothing
         | null xs   -> error "randomTermWith: no generator" 
         | otherwise -> 
              xs !! fst (randomR (0, length xs - 1) rng)
       where xs = examples ex

---------------------------------------------------------------
-- Exercise codes (unique identification)

data ExerciseCode = EC String String | NoCode
   deriving (Eq, Ord)

instance Show ExerciseCode where
   show (EC xs ys) = xs ++ "." ++ ys
   show NoCode     = "no code"

noCode :: ExerciseCode
noCode = NoCode

makeCode :: String -> String -> ExerciseCode
makeCode a b
   | null a || null b || any invalidCodeChar (a++b) =
        error $ "Invalid exercise code: " ++ show (EC a b)
   | otherwise = 
        EC (map toLower a) (map toLower b)
   
readCode :: String -> Maybe ExerciseCode
readCode xs =
   case break invalidCodeChar xs of
      (as, '.':bs) | all validCodeChar bs -> 
         return $ makeCode as bs
      _ -> Nothing

validCodeChar, invalidCodeChar :: Char -> Bool
validCodeChar c = isAlphaNum c || c `elem` "-_"
invalidCodeChar = not . validCodeChar

domain :: ExerciseCode -> String
domain (EC s _) = s
domain _        = []

identifier :: ExerciseCode -> String
identifier (EC _ s) = s
identifier _        = []

---------------------------------------------------------------
-- Rest
     
equivalenceContext :: Exercise a -> Context a -> Context a -> Bool
equivalenceContext ex a b = 
   case eqWithContext ex of
      Just f  -> f a b 
      Nothing -> fromMaybe False $ 
         liftM2 (equivalence ex) (fromContext a) (fromContext b)
    
prettyPrinterContext :: Exercise a -> Context a -> String
prettyPrinterContext ex = 
   maybe "<<invalid term>>" (prettyPrinter ex) . fromContext
    
getRule :: Monad m => Exercise a -> String -> m (Rule (Context a))
getRule ex s = 
   case filter ((==s) . name) (ruleset ex) of 
      [hd] -> return hd
      []   -> fail $ "Could not find ruleid " ++ s
      _    -> fail $ "Ambiguous ruleid " ++ s

showDerivation :: Exercise a -> a -> String
showDerivation ex a =
   case derivation tree of
      Just d  -> show (f d) ++ extra d
      Nothing -> prettyPrinterContext ex (root tree)
                 ++ "\n   =>\n<<no derivation>>"
 where
   tree = derivationTree (strategy ex) (inContext ex a)
   extra d =
      case fromContext (last (terms d)) of
         Nothing               -> "<<invalid term>>"
         Just a | isReady ex a -> ""
                | otherwise    -> "<<not ready>>"
   -- A bit of hack to show the delta between two environments, not including
   -- the location variable
   f d = let t:ts = map (Shown . prettyPrinterContext ex) (terms d)
             xs   = zipWith3 present (steps d) (drop 1 (terms d)) (terms d)
             present a x y = Shown (show a ++ extra)
              where env = deleteEnv "location" (diffEnv (getEnvironment x) (getEnvironment y))
                    extra | nullEnv env = "" 
                          | otherwise   = "\n      " ++ show env
         in newDerivation t (zip xs ts)

-- local helper datatype
data Shown = Shown String 

instance Show Shown where
   show (Shown s) = s

printDerivation :: Exercise a -> a -> IO ()
printDerivation ex = putStrLn . showDerivation ex
         
---------------------------------------------------------------
-- Checks for an exercise

checkExercise :: Show a => Exercise a -> IO ()
checkExercise ex =
   case testGenerator ex of 
      Nothing  -> return ()
      Just gen -> do
         putStrLn ("** " ++ show (exerciseCode ex))
         let check txt p = putLabel txt >> quickCheck p
         check "parser/pretty printer" $ forAll gen $
            checkParserPrettyEx ex   
         
         putStrLn "Soundness non-buggy rules" 
         forM_ (filter (not . isBuggyRule) $ ruleset ex) $ \r -> do 
            putLabel ("    " ++ name r)
            testRuleSmart (equivalenceContext ex) r (liftM (inContext ex) gen)

         check "non-trivial terms" $ 
            forAll gen $ \x -> 
            let trivial  = isReady ex x
                rejected = not trivial
                suitable = not trivial in
            classify trivial  "trivial"  $
            classify rejected "rejected" $
            classify suitable "suitable" $ property True 
         check "soundness strategy/generator" $ 
            forAll gen $
               maybe False (isReady ex) . fromContext . applyD (strategy ex) . inContext ex

-- check combination of parser and pretty-printer
checkParserPretty :: (a -> a -> Bool) -> (String -> Either b a) -> (a -> String) -> a -> Bool
checkParserPretty eq parser pretty a = 
   either (const False) (eq a) (parser (pretty a))

checkParserPrettyEx :: Exercise a -> a -> Bool
checkParserPrettyEx ex = 
   checkParserPretty (similarity ex) (parser ex) (prettyPrinter ex)

checksForList :: Exercise a -> IO ()
checksForList ex
   | status ex /= Experimental || null xs = return ()
   | otherwise = do
         let err s = putStrLn $ "Error: " ++ s
         putStrLn ("** " ++ show (exerciseCode ex))
         mapM_ (either err return . checksForTerm ex) xs
 where xs = examples ex

checksForTerm :: Monad m => Exercise a -> a -> m ()
checksForTerm ex a = 
   let txt = prettyPrinter ex a in
   case derivation (derivationTree (strategy ex) (inContext ex a)) of
      Nothing -> fail $ "no derivation for " ++ txt
      Just theDerivation -> do
         unless (maybe False (isReady ex) (fromContext (last as))) $
            fail $ "not solved: " ++ txt
         case [ (x, y) | x <- as, y <- as, not (equivalenceContext ex x y) ] of
            (x, y):_ -> fail $  "not equivalent: " 
                             ++ prettyPrinterContext ex x ++ "  and  "
                             ++ prettyPrinterContext ex y
            _        -> return ()
         case filter (maybe False (not . checkParserPrettyEx ex) . fromContext) as of
            hd:_ -> let s = prettyPrinterContext ex hd in
                    fail $ "parse error for " ++ s 
                         ++ ": parsed as " 
                         ++ either show (prettyPrinter ex) (parser ex s)
            _    -> return ()
       where
         as = terms theDerivation