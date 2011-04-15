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
   , exerciseId, status, parser, prettyPrinter
   , equivalence, similarity, isReady, isSuitable
   , splitParts
   , strategy, navigation, canBeRestarted, extraRules, ruleOrdering
   , difference, ordering, testGenerator, randomExercise, examples, getRule
   , simpleGenerator, useGenerator
   , randomTerm, randomTermWith, ruleset
   , makeContext, inContext, recognizeRule, ruleIsRecognized
   , ruleOrderingWith, ruleOrderingWithId
   , Examples, mapExamples, Difficulty(..), readDifficulty, level
     -- * Exercise status
   , Status(..), isPublic, isPrivate
     -- * Miscellaneous
   , withoutContext, simpleSimilarity, simpleEquivalence
   , prettyPrinterContext, restrictGenerator
   , showDerivation, printDerivation
   , ExerciseDerivation, defaultDerivation, derivationDiffEnv
   , checkExercise, checkParserPretty
   , checkExamples, exerciseTestSuite
   ) where

import Common.Classes
import Common.Context
import Common.Strategy hiding (not, fail, repeat, replicate)
import qualified Common.Strategy as S
import Common.Derivation
import Common.Id
import Common.Navigator
import Common.TestSuite
import Common.Transformation
import Common.Utils (ShowString(..), Some(..), commaList)
import Common.View (makeView)
import Control.Monad.Error
import Control.Arrow
import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import Data.Ord
import System.Random
import Test.QuickCheck hiding (label)
import Test.QuickCheck.Gen

data Exercise a = Exercise
   { -- identification and meta-information
     exerciseId     :: Id -- identifier that uniquely determines the exercise
   , status         :: Status
     -- parsing and pretty-printing
   , parser         :: String -> Either String a
   , prettyPrinter  :: a -> String
     -- syntactic and semantic checks
   , equivalence    :: Context a -> Context a -> Bool
   , similarity     :: Context a -> Context a -> Bool -- possibly more liberal than syntactic equality
   , ordering       :: a -> a -> Ordering  -- syntactic comparison
   , isReady        :: a -> Bool
   , isSuitable     :: a -> Bool
   , difference     :: Bool -> a -> a -> Maybe (a, a)
   , splitParts     :: a -> [a]
     -- strategies and rules
   , strategy       :: LabeledStrategy (Context a)
   , navigation     :: a -> Navigator a
   , canBeRestarted :: Bool                -- By default, assumed to be the case
   , extraRules     :: [Rule (Context a)]  -- Extra rules (possibly buggy) not appearing in strategy
   , ruleOrdering   :: Rule (Context a) -> Rule (Context a) -> Ordering -- Ordering on rules (for onefirst)
     -- testing and exercise generation
   , testGenerator  :: Maybe (Gen a)
   , randomExercise :: Maybe (StdGen -> Difficulty -> a)
   , examples       :: [(Difficulty, a)]
   }

instance Eq (Exercise a) where
   e1 == e2 = getId e1 == getId e2

instance Ord (Exercise a) where
   compare = comparing getId

instance Apply Exercise where
   applyAll ex = concatMap fromContext . applyAll (strategy ex) . inContext ex

instance HasId (Exercise a) where
   getId = exerciseId
   changeId f ex = ex { exerciseId = f (exerciseId ex) }

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
     exerciseId     = error "no exercise code"
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
   , splitParts     = return
     -- strategies and rules
   , strategy       = label "Fail" S.fail
   , navigation     = noNavigator
   , canBeRestarted = True
   , extraRules     = []
   , ruleOrdering   = compareId
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
-- Difficulty levels

type Examples a = [(Difficulty, a)]

mapExamples :: (a -> b) -> Examples a -> Examples b
mapExamples f = map (second f)

data Difficulty = VeryEasy | Easy | Medium | Difficult | VeryDifficult
   deriving (Eq, Ord, Enum)
   
instance Show Difficulty where
   show = (xs !!) . fromEnum 
    where 
      xs = ["very_easy", "easy", "medium", "difficult", "very_difficult"]

readDifficulty :: String -> Maybe Difficulty
readDifficulty s =
   case filter p [VeryEasy .. VeryDifficult] of
            [a] -> Just a
            _   -> Nothing
 where
   normal = filter isAlpha . map toLower
   p = ((== normal s) . normal . show)

level :: Difficulty -> [a] -> Examples a
level = zip . repeat

---------------------------------------------------------------
-- Exercise generators

-- returns a sorted list of rules (no duplicates)
ruleset :: Exercise a -> [Rule (Context a)]
ruleset ex = nub (sortBy compareId list)
 where 
   list = rulesInStrategy (strategy ex) ++ extraRules ex
 
simpleGenerator :: Gen a -> Maybe (StdGen -> Difficulty -> a) 
simpleGenerator = useGenerator (const True) . const

useGenerator :: (a -> Bool) -> (Difficulty -> Gen a) -> Maybe (StdGen -> Difficulty -> a) 
useGenerator p makeGen = Just (\rng -> rec rng . makeGen)
 where
   rec rng gen@(MkGen f)
      | p a       = a
      | otherwise = rec (snd (next rng)) gen
    where
      (size, r) = randomR (0, 100) rng
      a         = f r size

restrictGenerator :: (a -> Bool) -> Gen a -> Gen a
restrictGenerator p g = do
   a <- g 
   if p a then return a 
          else restrictGenerator p g

randomTerm :: Difficulty -> Exercise a -> IO a
randomTerm dif ex = do
   rng <- newStdGen
   return (randomTermWith rng dif ex)

randomTermWith :: StdGen -> Difficulty -> Exercise a -> a
randomTermWith rng dif ex =
   case randomExercise ex of
      Just f  -> f rng dif
      Nothing
         | null xs   -> error "randomTermWith: no generator" 
         | otherwise -> 
              snd (xs !! fst (randomR (0, length xs - 1) rng))
       where xs = examples ex

ruleIsRecognized :: Exercise a -> Rule (Context a) -> Context a -> Context a -> Bool
ruleIsRecognized ex r ca = not . null . recognizeRule ex r ca

-- Recognize a rule at (possibly multiple) locations
recognizeRule :: Exercise a -> Rule (Context a) -> Context a -> Context a -> [Location]
recognizeRule ex r ca cb = rec (fromMaybe ca (top ca))
 where
   rec x = [ location x | here r x cb ] ++ concatMap rec (allDowns x)
   here  = ruleRecognizer (similarity ex)

ruleOrderingWith :: [Rule a] -> Rule a -> Rule a -> Ordering
ruleOrderingWith = ruleOrderingWithId . map getId
 
ruleOrderingWithId :: HasId b => [b] -> Rule a -> Rule a -> Ordering
ruleOrderingWithId bs r1 r2 =
   let xs = map getId bs in
   case (findIndex (==getId r1) xs, findIndex (==getId r2) xs) of
      (Just i,  Just j ) -> i `compare` j
      (Just _,  Nothing) -> LT
      (Nothing, Just _ ) -> GT
      (Nothing, Nothing) -> compareId r1 r2

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
-- Rest

-- | Function for defining equivalence or similarity without taking
-- the context into account. 
withoutContext :: (a -> a -> Bool) -> Context a -> Context a -> Bool
withoutContext f a b = fromMaybe False (fromContextWith2 f a b)

-- | Similarity on terms without a context
simpleSimilarity :: Exercise a -> a -> a -> Bool
simpleSimilarity ex = similarity ex `on` inContext ex

-- | Equivalence on terms without a context
simpleEquivalence :: Exercise a -> a -> a -> Bool
simpleEquivalence ex = equivalence ex `on` inContext ex

prettyPrinterContext :: Exercise a -> Context a -> String
prettyPrinterContext ex = 
   maybe "<<invalid term>>" (prettyPrinter ex) . fromContext
    
getRule :: Monad m => Exercise a -> Id -> m (Rule (Context a))
getRule ex a = 
   case filter ((a ==) . getId) (ruleset ex) of 
      [hd] -> return hd
      []   -> fail $ "Could not find ruleid " ++ showId a
      _    -> fail $ "Ambiguous ruleid " ++ showId a

-- |Shows a derivation for a given start term. The specified rule ordering
-- is used for selection.
showDerivation :: Exercise a -> a -> String
showDerivation ex a = show (present der) ++ extra
 where
   der   = derivationPrevious (derivationDiffEnv (defaultDerivation ex a))
   extra =
      case fromContext (last (terms der)) of
         Nothing               -> "<<invalid term>>"
         Just b | isReady ex b -> ""
                | otherwise    -> "<<not ready>>"
   present = mapStepsDerivation (ShowString . f) 
           . fmap (ShowString . prettyPrinterContext ex)
   f ((b, env), old) = showId b ++ part1 ++ part2
    where 
      newl = "\n      "
      g (Some descr) x = labelArgument descr ++ "=" ++ x
      part1 = case expectedArguments b old of
                 Just xs -> newl ++ commaList (zipWith g (getDescriptors b) xs)
                 Nothing -> ""
      part2 | nullEnv env = "" 
            | otherwise   = newl ++ show env

type ExerciseDerivation a = Derivation (Rule (Context a)) (Context a)

defaultDerivation :: Exercise a -> a -> ExerciseDerivation a
defaultDerivation ex a =
   let ca     = inContext ex a
       tree   = sortTree (ruleOrdering ex) (derivationTree (strategy ex) ca)
       single = newDerivation ca []
   in fromMaybe single (derivation tree)

derivationDiffEnv :: Derivation s (Context a) -> Derivation (s, Environment) (Context a)
derivationDiffEnv d =
   -- A bit of hack to show the delta between two environments, not including
   -- the location variable
   let t:ts = terms d
       xs   = zipWith3 f (steps d) (drop 1 (terms d)) (terms d)
       f b x y = (b, deleteEnv "location" (diffEnv (getEnvironment x) (getEnvironment y))) -- ShowString (show a ++ extra)
   in newDerivation t (zip xs ts)

-- helper, needed for showing arguments
derivationPrevious :: Derivation s a -> Derivation (s, a) a
derivationPrevious d =
   let t:ts = terms d  
   in newDerivation t (zip (zip (steps d) (t:ts)) ts)

printDerivation :: Exercise a -> a -> IO ()
printDerivation ex = putStrLn . showDerivation ex
         
---------------------------------------------------------------
-- Checks for an exercise

checkExercise :: Exercise a -> IO ()
checkExercise = runTestSuite . exerciseTestSuite

exerciseTestSuite :: Exercise a -> TestSuite
exerciseTestSuite ex = suite ("Exercise " ++ show (exerciseId ex)) $ do
   -- get some exercises
   xs <- if isJust (randomExercise ex)
         then liftIO $ replicateM 10 (randomTerm Medium ex)
         else return (map snd (examples ex))
   -- do tests
   assertTrue "Exercise terms defined" (not (null xs))
   assertTrue "Equivalence implemented" $
      let eq a b = equivalence ex (inContext ex a) (inContext ex b)
      in length (nubBy eq xs) > 1
   assertTrue "Similarity implemented" $
      let sim a b = similarity ex (inContext ex a) (inContext ex b)
      in length (nubBy sim xs) > 1
   checkExamples ex
   case testGenerator ex of 
      Nothing  -> return ()
      Just gen -> do
         let showAsGen = showAs (prettyPrinter ex) gen
         addProperty "parser/pretty printer" $ forAll showAsGen $
            checkParserPrettyEx ex . inContext ex . fromS

         suite "Soundness non-buggy rules" $
            forM_ (filter (not . isBuggyRule) $ ruleset ex) $ \r -> 
               let eq a b = equivalence ex (fromS a) (fromS b)
                   myGen  = showAs (prettyPrinterContext ex) (liftM (inContext ex) gen)
                   myView = makeView (return . fromS) (S (prettyPrinterContext ex))
                   args   = stdArgs {maxSize = 10, maxSuccess = 10, maxDiscard = 100}
               in addPropertyWith (showId r) args $ 
                     propRuleSmart eq (liftRule myView r) myGen 
 
         addProperty "soundness strategy/generator" $ 
            forAll showAsGen $
               maybe False (isReady ex) . fromContext
               . applyD (strategy ex) . inContext ex . fromS

data ShowAs a = S {showS :: a -> String, fromS :: a}

instance Show (ShowAs a) where
   show a = showS a (fromS a)

showAs :: (a -> String) -> Gen a -> Gen (ShowAs a)
showAs f = liftM (S f)

-- check combination of parser and pretty-printer
checkParserPretty :: (a -> a -> Bool) -> (String -> Either String a) -> (a -> String) -> a -> Bool
checkParserPretty eq p pretty a = 
   either (const False) (eq a) (p (pretty a))

checkParserPrettyEx :: Exercise a -> Context a -> Bool
checkParserPrettyEx ex = 
   let f = either Left (Right . inContext ex) . parser ex
   in checkParserPretty (similarity ex) f (prettyPrinterContext ex)

checkExamples :: Exercise a -> TestSuite
checkExamples ex = do
   let xs = map snd (examples ex)
   unless (null xs) $ suite "Examples" $
      mapM_ (checksForTerm True ex) xs

checksForTerm :: Bool -> Exercise a -> a -> TestSuite
checksForTerm leftMost ex a = do
   let tree = derivationTree (strategy ex) (inContext ex a)
   -- Left-most derivation
   when leftMost $
      case derivation tree of
         Just d  -> checksForDerivation ex d
         Nothing -> 
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
       p1  = not . checkParserPrettyEx ex
   assertNull "parser/pretty-printer" $ take 1 $ flip map (filter p1 ts) $ \hd -> 
      let s = prettyPrinterContext ex hd 
      in "parse error for " ++ s ++ ": parsed as " 
         ++ either show (prettyPrinter ex) (parser ex s)


   -- Equivalences between terms
   let pairs    = [ (x, y) | x <- ts, y <- ts ]
       p2 (x, y) = not (equivalence ex x y)
   assertNull "equivalences" $ take 1 $ flip map (filter p2 pairs) $ \(x, y) ->
      "not equivalent: " ++ prettyPrinterContext ex x
      ++ "  with  " ++ prettyPrinterContext ex y

   -- Similarity of terms
   let p3 (x, _, y) = similarity ex x y
   assertNull  "similars" $ take 1 $ flip map (filter p3 (triples d)) $ \(x, r, y) -> 
      "similar subsequent terms: " ++ prettyPrinterContext ex x
      ++ "  with  " ++ prettyPrinterContext ex y
      ++ "  using  " ++ show r
               
   let xs = [ x | x <- terms d, not (similarity ex x x) ]
   assertNull "self similarity" $ take 1 $ flip map xs $ \hd -> 
      "term not similar to itself: " ++ prettyPrinterContext ex hd