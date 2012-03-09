{-# LANGUAGE Rank2Types #-}
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
-- This module defines the concept of an exercise
--
-----------------------------------------------------------------------------
module Common.Exercise
   ( -- * Exercises
     Exercise, makeExercise, emptyExercise
   , exerciseId, status, parser, prettyPrinter
   , equivalence, similarity, ready, suitable, isReady, isSuitable
   , hasTermView
   , strategy, navigation, canBeRestarted, extraRules, ruleOrdering
   , difference, differenceEqual
   , testGenerator, randomExercise, examples, getRule
   , simpleGenerator, useGenerator
   , randomTerm, randomTermWith, ruleset
   , makeContext, inContext, recognizeRule
   , ruleOrderingWith, ruleOrderingWithId
   , Examples, mapExamples, Difficulty(..), readDifficulty, level
   , hasTypeable, useTypeable, castFrom, castTo
     -- * Exercise status
   , Status(..), isPublic, isPrivate
     -- * Miscellaneous
   , withoutContext, simpleSimilarity, simpleEquivalence
   , prettyPrinterContext, restrictGenerator
   , showDerivation, printDerivation
   , ExerciseDerivation, defaultDerivation
   , derivationDiffEnv
   , checkExercise, checkParserPretty
   , checkExamples, exerciseTestSuite
   ) where

import Common.Environment
import Common.Classes
import Common.Context
import Common.Derivation
import Common.DerivationTree
import Common.Id
import Common.Traversal.Navigator (top, downs)
import Common.Predicate
import Common.Rewriting
import Common.Strategy hiding (not, fail, repeat, replicate)
import Common.Rule
import Common.Utils (ShowString(..))
import Common.Utils.TestSuite
import Common.View
import Control.Monad.Error
import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import Data.Ord
import Data.Typeable
import System.Random
import Test.QuickCheck hiding (label)
import Test.QuickCheck.Gen
import qualified Common.Rewriting.Difference as Diff
import qualified Common.Strategy as S

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
   , ready          :: Predicate a
   , suitable       :: Predicate a
   , hasTermView    :: Maybe (View Term a)
   , hasTypeable    :: Maybe (IsTypeable a)
     -- strategies and rules
   , strategy       :: LabeledStrategy (Context a)
   , navigation     :: a -> ContextNavigator a
   , canBeRestarted :: Bool                -- By default, assumed to be the case
   , extraRules     :: [Rule (Context a)]  -- Extra rules (possibly buggy) not appearing in strategy
   , ruleOrdering   :: Rule (Context a) -> Rule (Context a) -> Ordering -- Ordering on rules (for onefirst)
     -- testing and exercise generation
   , testGenerator  :: Maybe (Gen a)
   , randomExercise :: Maybe (StdGen -> Maybe Difficulty -> a)
   , examples       :: [(Difficulty, a)]
   }

instance Eq (Exercise a) where
   e1 == e2 = getId e1 == getId e2

instance Ord (Exercise a) where
   compare = comparing getId

instance Apply Exercise where
   applyAll ex = mapMaybe fromContext . applyAll (strategy ex) . inContext ex

instance HasId (Exercise a) where
   getId = exerciseId
   changeId f ex = ex { exerciseId = f (exerciseId ex) }

makeExercise :: (Show a, Eq a, IsTerm a) => Exercise a
makeExercise = emptyExercise
   { prettyPrinter = show
   , similarity    = (==)
   , hasTermView   = Just termView
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
   , ready          = true
   , suitable       = true
   , hasTermView    = Nothing
   , hasTypeable    = Nothing
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
inContext = flip makeContext mempty

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
   p = (== normal s) . normal . show

level :: Difficulty -> [a] -> Examples a
level = zip . repeat

---------------------------------------------------------------
-- Exercise generators

-- returns a sorted list of rules (no duplicates)
ruleset :: Exercise a -> [Rule (Context a)]
ruleset ex = nub (sortBy compareId list)
 where
   list = extraRules ex ++ rulesInStrategy (strategy ex)

simpleGenerator :: Gen a -> Maybe (StdGen -> Maybe Difficulty -> a)
simpleGenerator = useGenerator (const True) . const

useGenerator :: (a -> Bool) -> (Maybe Difficulty -> Gen a) -> Maybe (StdGen -> Maybe Difficulty -> a)
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

randomTerm :: Exercise a -> Maybe Difficulty -> IO a
randomTerm ex mdif = do
   rng <- newStdGen
   maybe (fail "no random term") return $ randomTermWith rng ex mdif

randomTermWith :: StdGen -> Exercise a -> Maybe Difficulty -> Maybe a
randomTermWith rng ex mdif =
   case randomExercise ex of
      Just f  -> return (f rng mdif)
      Nothing
         | null xs   -> Nothing
         | otherwise -> Just $
              snd $ xs !! fst (randomR (0, length xs - 1) rng)
       where 
         xs = filter p (examples ex)
         p (d, _) = maybe True (==d) mdif

difference :: Exercise a -> a -> a -> Maybe (a, a)
difference ex a b = do
   v <- hasTermView ex
   Diff.differenceWith v a b

differenceEqual :: Exercise a -> a -> a -> Maybe (a, a)
differenceEqual ex a b = do
   v <- hasTermView ex
   Diff.differenceEqualWith v (simpleEquivalence ex) a b

-- Recognize a rule at (possibly multiple) locations
recognizeRule :: Exercise a -> Rule (Context a) -> Context a -> Context a -> [(Location, Environment)]
recognizeRule ex r ca cb = rec (top ca)
 where
   final = addTransRecognizer (similarity ex) r
   rec x = do
      -- here
      as <- recognizeAll final x cb
      return (location x, as)
    `mplus` -- or there
      concatMap rec (downs x)

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
-- Using type representations for casts

data IsTypeable a = IT (forall b . Typeable b => a -> Maybe b)
                       (forall b . Typeable b => b -> Maybe a)

useTypeable :: Typeable a => Maybe (IsTypeable a)
useTypeable = Just (IT cast cast)

castFrom :: Typeable b => Exercise a -> a -> Maybe b
castFrom ex a = do
   IT f _ <- hasTypeable ex
   f a

castTo :: Typeable b => Exercise a -> b -> Maybe a
castTo ex a = do
   IT _ g <- hasTypeable ex
   g a

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

isReady :: Exercise a -> a -> Bool
isReady = evalPredicate . ready

isSuitable :: Exercise a -> a -> Bool
isSuitable = evalPredicate . suitable

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
   der   = derivationDiffEnv (defaultDerivation ex a)
   extra =
      case fromContext (lastTerm der) of
         Nothing               -> "<<invalid term>>"
         Just b | isReady ex b -> ""
                | otherwise    -> "<<not ready>>"
   present = biMap (ShowString . f) (ShowString . prettyPrinterContext ex)
   f ((r, local), global) = showId r ++ part1 ++ part2
    where
      newl  = "\n      "
      part1 = newl ++ show local
      part2 | noBindings global = ""
            | otherwise         = newl ++ show global

type ExerciseDerivation a = Derivation (Rule (Context a), Environment) (Context a)

defaultDerivation :: Exercise a -> a -> ExerciseDerivation a
defaultDerivation ex a =
   let ca     = inContext ex a
       tree   = sortTree (ruleOrdering ex `on` fst) (derivationTree (strategy ex) ca)
       single = emptyDerivation ca
   in fromMaybe single (derivation tree)

derivationDiffEnv :: Derivation s (Context a) -> Derivation (s, Environment) (Context a)
derivationDiffEnv = updateSteps $ \old a new ->
   let keep x = not (getId x `sameId` "location" || x `elem` list)
       list = bindings old
   in (a, makeEnvironment $ filter keep $ bindings new)

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
         then liftIO $ replicateM 10 (randomTerm ex Nothing)
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

         {-
         suite "Soundness non-buggy rules" $
            forM_ (filter (not . isBuggyRule) $ ruleset ex) $ \r ->
               let eq a b = equivalence ex (fromS a) (fromS b)
                   myGen  = showAs (prettyPrinterContext ex) (liftM (inContext ex) gen)
                   myView = makeView (return . fromS) (S (prettyPrinterContext ex))
                   args   = stdArgs {maxSize = 10, maxSuccess = 10, maxDiscard = 100}
               in addPropertyWith (showId r) args $
                     propRuleSmart eq (liftView myView r) myGen -}

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
checkParserPrettyEx ex ca =
   let f    = mapSecond make . parser ex
       make = newContext (environment ca) . navigation ex
   in checkParserPretty (similarity ex) f (prettyPrinterContext ex) ca

{-
propRule :: Show a => (a -> a -> Bool) -> Rule a -> Gen a -> Property
propRule eq r gen =
   forAll gen $ \a ->
   let xs = applyAll r a in 
   not (null xs) ==> 
   forAll (elements xs) $ \b -> 
   a `eq` b -}

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

checksForDerivation :: Exercise a -> Derivation (Rule (Context a), Environment) (Context a) -> TestSuite
checksForDerivation ex d = do
   -- Conditions on starting term
   let start = firstTerm d
   assertTrue
      ("start term not suitable: " ++ prettyPrinterContext ex start) $
      maybe False (isSuitable ex) (fromContext start)

   {-
   b2 <- do let b = False -- maybe True (isReady ex) (fromContext start)
            when b $ report $
               "start term is ready: " ++ prettyPrinterContext ex start
            return b-}
   -- Conditions on final term
   let final = lastTerm d
   {-
   b3 <- do let b = False -- maybe True (isSuitable ex) (fromContext final)
            when b $ report $
               "final term is suitable: " ++ prettyPrinterContext ex start
               ++ "  =>  " ++ prettyPrinterContext ex final
            return b -}
   assertTrue
      ("final term not ready: " ++ prettyPrinterContext ex start
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
   let p3 (x, (_, _), y) = similarity ex x y &&
                           on (==) (maybe False (isReady ex) . fromContext) x y
   assertNull  "similars" $ take 1 $ flip map (filter p3 (triples d)) $ \(x, r, y) ->
      "similar subsequent terms: " ++ prettyPrinterContext ex x
      ++ "  with  " ++ prettyPrinterContext ex y
      ++ "  using  " ++ show r

   assertNull "self similarity" $ take 1 $ do 
      x <- terms d
      guard (not (similarity ex x x))
      return $ "term not similar to itself: " ++ prettyPrinterContext ex x
      
   -- Parameters 
   assertNull "parameters" $ take 1 $ do
      (r, env) <- steps d
      maybeToList (checkReferences r env)