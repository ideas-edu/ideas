module Ideas.Common.ExerciseTests
   ( checkExercise, checkParserPretty
   , checkExamples, exerciseTestSuite
   ) where
   
import Control.Monad
import Data.Function
import Data.List
import Data.Maybe
import Ideas.Common.Classes
import Ideas.Common.Context
import Ideas.Common.Derivation
import Ideas.Common.DerivationTree
import Ideas.Common.Environment
import Ideas.Common.Exercise
import Ideas.Common.Rule
import Ideas.Common.Strategy (derivationTree)
import Ideas.Common.Utils.TestSuite
import System.Random
import Test.QuickCheck
   
---------------------------------------------------------------
-- Checks for an exercise

checkExercise :: Exercise a -> IO ()
checkExercise = runTestSuite . exerciseTestSuite

exerciseTestSuite :: Exercise a -> TestSuite
exerciseTestSuite ex = suite ("Exercise " ++ show (exerciseId ex)) $ do
   rng <- liftIO getStdGen
   -- get some exercises
   let xs | isJust (randomExercise ex) =
               take 10 (randomTerms rng ex Nothing)
          | otherwise = map snd (examples ex)
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
   let tree = derivationTree False (strategy ex) (inContext ex a)
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