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
-- This module defines the concept of an exercise
--
-----------------------------------------------------------------------------
module Common.Exercise 
   ( -- * Exercises
     Exercise, testableExercise, makeExercise, emptyExercise
   , Id.description, Id.describe, exerciseCode, status, parser, prettyPrinter
   , equivalence, similarity, isReady, isSuitable, eqWithContext
   , strategy, navigation, canBeRestarted, extraRules, ruleOrdering
   , difference, ordering, testGenerator, randomExercise, examples, getRule
   , simpleGenerator, useGenerator
   , randomTerm, randomTermWith, ruleset
   , makeContext, inContext, recognizeRule, ruleIsRecognized
   , ruleOrderingWith, ruleNameOrderingWith
     -- * Exercise status
   , Status(..), isPublic, isPrivate
     -- * Exercise codes
   , ExerciseCode, makeCode, readCode, domain, identifier
     -- * Miscellaneous
   , equivalenceContext, restrictGenerator
   , showDerivation, printDerivation
   , checkExercise, checkParserPretty
   , checkExamples, generate, exerciseTestSuite
   ) where

import Common.Apply
import Common.Context
import Common.Strategy hiding (not, fail, replicate)
import qualified Common.Strategy as S
import Common.Derivation
import qualified Common.Id as Id
import Common.Navigator
import Common.TestSuite
import Common.Transformation
import Common.View (makeView)
import Control.Monad.Error
import Data.Char
import Data.List
import Data.Maybe
import System.Random
import Test.QuickCheck hiding (label)
import Test.QuickCheck.Gen

data Exercise a = Exercise
   { -- identification and meta-information
     exerciseCode   :: ExerciseCode -- uniquely determines the exercise (in a given domain)
   , status         :: Status
     -- parsing and pretty-printing
   , parser         :: String -> Either String a
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
   , ruleOrdering   :: Rule (Context a) -> Rule (Context a) -> Ordering -- Ordering on rules (for onefirst)
     -- testing and exercise generation
   , testGenerator  :: Maybe (Gen a)
   , randomExercise :: Maybe (StdGen -> Int -> a)
   , examples       :: [a]
   }

instance Eq (Exercise a) where
   e1 == e2 = exerciseCode e1 == exerciseCode e2

instance Ord (Exercise a) where
   e1 `compare` e2 = exerciseCode e1 `compare` exerciseCode e2

instance Apply Exercise where
   applyAll ex = concatMap fromContext . applyAll (strategy ex) . inContext ex

instance Id.HasId (Exercise a) where
   getId = exerciseCode
   changeId f ex = ex { exerciseCode = f (exerciseCode ex) }

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
     exerciseCode   = error "no exercise code"
   , status         = Experimental
     -- parsing and pretty-printing
   , parser         = const (Left "<<no parser>>")
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
   , ruleOrdering   = \r1 r2 -> getId r1 `compare` getId r2
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
   cmp a b = getId a `compare` getId b
 
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
        where

generate :: Int -> StdGen -> Gen a -> a
generate n rnd (MkGen m) = m rnd' size
  where
    (size, rnd') = randomR (0, n) rnd

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

ruleIsRecognized :: Exercise a -> Rule (Context a) -> Context a -> Context a -> Bool
ruleIsRecognized ex r ca = not . null . recognizeRule ex r ca

-- Recognize a rule at (possibly multiple) locations
recognizeRule :: Exercise a -> Rule (Context a) -> Context a -> Context a -> [Location]
recognizeRule ex r ca cb = rec (fromMaybe ca (top ca))
 where
   rec x = [ location x | here r x cb ] ++ concatMap rec (allDowns x)
   here  = ruleRecognizer $ \cx cy -> fromMaybe False $
      liftM2 (similarity ex) (fromContext cx) (fromContext cy)

ruleOrderingWith :: [Rule a] -> Rule a -> Rule a -> Ordering
ruleOrderingWith = ruleNameOrderingWith . map showId
 
ruleNameOrderingWith :: [String] -> Rule a -> Rule a -> Ordering
ruleNameOrderingWith xs r1 r2 =
   case (findIndex (==showId r1) xs, findIndex (==showId r2) xs) of
      (Just i,  Just j ) -> i `compare` j
      (Just _,  Nothing) -> LT
      (Nothing, Just _ ) -> GT
      (Nothing, Nothing) -> getId r1 `compare` getId r2

---------------------------------------------------------------
-- Exercise status

data Status 
   = Stable       -- ^ A released exercise that has undergone some thorough testing
   | Provisional  -- ^ A released exercise, possibly with some deficiencies
   | Alpha        -- ^ An exercise that is under development
   | Experimental -- ^ An exercise for experimentation purposes only
   deriving (Show, Eq)

-- | An exercise with the status @Stable@ or @Provisional@
isPublic :: Exercise a -> Bool
isPublic ex = status ex `elem` [Stable, Provisional]

-- | An exercise that is not public
isPrivate :: Exercise a -> Bool
isPrivate = not . isPublic

---------------------------------------------------------------
-- Exercise codes (unique identification)

type ExerciseCode = Id.Id

makeCode :: String -> String -> ExerciseCode
makeCode a b
   | null a || null b || any invalidCodeChar (a++b) =
        error $ "Invalid exercise code: " ++ show (Id.newQId Id.IdExercise a b)
   | otherwise = 
        Id.newQId Id.IdExercise (map toLower a) (map toLower b)
   
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
domain = concat . intersperse "." . Id.quantifiers

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
   case filter ((==s) . showId) (ruleset ex) of 
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
{-
checkExercise :: Exercise a -> IO ()
checkExercise ex = do 
   putStrLn ("** " ++ show (exerciseCode ex))
   -- Derivations for examples
   checkExamples ex
   -- Derivations for test generator
   case testGenerator ex of
      Nothing  -> return ()
      Just gen -> do 
         putStrLn "Checking with test generator"
         forM_ [0 .. 100] $ \i -> do 
            -- putChar '.'
            g <- newStdGen
            checksForTerm False ex (generate i g gen)
            return ()
   -- Derivations for random exercise generator
   case randomExercise ex of
      Nothing  -> return ()
      Just f -> do 
         putStrLn "Checking with random exercise generator"
         forM_ [0 .. 109] $ \i -> do 
            -- putChar '.'
            g <- newStdGen
            checksForTerm False ex (f g (i `div` 10))
            return ()
   -- Soundness of rules
   case testGenerator ex of
      Nothing  -> return ()
      Just gen -> do
         putStrLn "Soundness of rules with test generator"
         forM_ (filter (not . isBuggyRule) (ruleset ex)) $ \r -> do
            putStr ("[" ++ show r ++ "]   ")
            xs <- generateIO 300 (smartGen r (liftM (inContext ex) gen))
            let list = [ (x, y) | x <- xs, y <- applyAll r x ]
                p (x, y) = not (equivalenceContext ex x y)               
            case filter p list of
               [] | null list -> putStrLn "Warning: no applications found" 
                  | otherwise -> putStrLn "Ok"
               (x, y):_ -> report $ 
                  "counter example: " ++ prettyPrinterContext ex x
                  ++ "  =>  " ++ prettyPrinterContext ex y -}

checkExercise :: Exercise a -> IO ()
checkExercise = runTestSuite . exerciseTestSuite

exerciseTestSuite :: Exercise a -> TestSuite
exerciseTestSuite ex = suite ("Exercise " ++ show (exerciseCode ex)) $ do
   checkExamples ex
   case testGenerator ex of 
      Nothing  -> return ()
      Just gen -> do
         let showAsGen = showAs (prettyPrinter ex) gen
         addProperty "parser/pretty printer" $ forAll showAsGen $
            checkParserPrettyEx ex . from

         suite "Soundness non-buggy rules" $ do
            forM_ (filter (not . isBuggyRule) $ ruleset ex) $ \r -> 
               let eq a b = equivalenceContext ex (from a) (from b)
                   myGen  = showAs (prettyPrinterContext ex) (liftM (inContext ex) gen)
                   myView = makeView (return . from) (S (prettyPrinterContext ex))
                   args   = stdArgs {maxSize = 10, maxSuccess = 10, maxDiscard = 100}
               in addPropertyWith (showId r) args $ 
                     propRuleSmart eq (liftRule myView r) myGen 
 
         addProperty "soundness strategy/generator" $ 
            forAll showAsGen $
               maybe False (isReady ex) . fromContext
               . applyD (strategy ex) . inContext ex . from

data ShowAs a = S {showS :: a -> String, from :: a}

instance Show (ShowAs a) where
   show a = showS a (from a)

showAs :: (a -> String) -> Gen a -> Gen (ShowAs a)
showAs f = liftM (S f)

-- check combination of parser and pretty-printer
checkParserPretty :: (a -> a -> Bool) -> (String -> Either b a) -> (a -> String) -> a -> Bool
checkParserPretty eq parser pretty a = 
   either (const False) (eq a) (parser (pretty a))

checkParserPrettyEx :: Exercise a -> a -> Bool
checkParserPrettyEx ex = 
   checkParserPretty (similarity ex) (parser ex) (prettyPrinter ex)

checkExamples :: Exercise a -> TestSuite
checkExamples ex = do
   let xs = examples ex
   unless (null xs) $ suite "Examples" $
      mapM_ (checksForTerm True ex) xs

checksForTerm :: Bool -> Exercise a -> a -> TestSuite
checksForTerm leftMost ex a = do
   let tree = derivationTree (strategy ex) (inContext ex a)
   -- Left-most derivation
   if not leftMost then return () else
      case derivation tree of
         Just d  -> checksForDerivation ex d
         Nothing -> do 
            fail $ "no derivation for " ++ prettyPrinter ex a
   -- Random derivation
   g <- liftIO getStdGen
   case randomDerivation g tree of
      Just d  -> checksForDerivation ex d
      Nothing -> return () 
         
checksForDerivation :: Exercise a -> Derivation (Rule (Context a)) (Context a) -> TestSuite
checksForDerivation ex d = do
   -- Conditions on starting term
   let start = head (terms d)
   assertTrueMsg "start term" 
      ("not suitable: " ++ prettyPrinterContext ex start) $
      maybe False (isSuitable ex) (fromContext start)
   
   {-
   b2 <- do let b = False -- maybe True (isReady ex) (fromContext start)
            when b $ report $ 
               "start term is ready: " ++ prettyPrinterContext ex start
            return b-}
   -- Conditions on final term
   let final = last (terms d)
   {-
   b3 <- do let b = False -- maybe True (isSuitable ex) (fromContext final)
            when b $ report $ 
               "final term is suitable: " ++ prettyPrinterContext ex start
               ++ "  =>  " ++ prettyPrinterContext ex final
            return b -}
   assertTrueMsg "final term" 
      ("not ready: " ++ prettyPrinterContext ex start
               ++ "  =>  " ++ prettyPrinterContext ex final) $ 
      maybe False (isReady ex) (fromContext final)

   -- Parser/pretty printer on terms
   let ts  = terms d
       p   = maybe False (not . checkParserPrettyEx ex) . fromContext
   assertNull "parser/pretty-printer" $ take 1 $ flip map (filter p ts) $ \hd -> 
      let s = prettyPrinterContext ex hd 
      in "parse error for " ++ s ++ ": parsed as " 
         ++ either show (prettyPrinter ex) (parser ex s)


   -- Equivalences between terms
   let pairs    = [ (x, y) | x <- ts, y <- ts ]
       p (x, y) = not (equivalenceContext ex x y)
   assertNull "equivalences" $ take 1 $ flip map (filter p pairs) $ \(x, y) ->
      "not equivalent: " ++ prettyPrinterContext ex x
      ++ "  with  " ++ prettyPrinterContext ex y

   -- Similarity of terms
   let p (x, _, y) = fromMaybe False $ 
                        liftM2 (similarity ex) (fromContext x) (fromContext y)
   assertNull  "similars" $ take 1 $ flip map (filter p (triples d)) $ \(x, r, y) -> 
      "similar subsequent terms: " ++ prettyPrinterContext ex x
      ++ "  with  " ++ prettyPrinterContext ex y
      ++ "  using  " ++ show r
               
   let xs = [ x | cx <- terms d, x <- fromContext cx, not (similarity ex x x) ]
   assertNull "self similarity" $ take 1 $ flip map xs $ \hd -> 
      "term not similar to itself: " ++ prettyPrinter ex hd