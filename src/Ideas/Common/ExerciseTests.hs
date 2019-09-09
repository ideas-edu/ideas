-----------------------------------------------------------------------------
-- Copyright 2019, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------

module Ideas.Common.ExerciseTests where

import Control.Monad
import Data.Function
import Data.List
import Data.Maybe
import Ideas.Common.Classes
import Ideas.Common.Context
import Ideas.Common.Derivation
import Ideas.Common.Environment
import Ideas.Common.Exercise
import Ideas.Common.Id
import Ideas.Common.Rule
import Ideas.Common.View
import Ideas.Utils.TestSuite
import Test.QuickCheck
import Test.QuickCheck.Random

---------------------------------------------------------------
-- Checks for an exercise

checkExercise :: Exercise a -> IO ()
checkExercise ex = do
   qcgen <- newQCGen
   runTestSuite True (exerciseTestSuite qcgen ex)

exerciseTestSuite :: QCGen -> Exercise a -> TestSuite
exerciseTestSuite qcgen ex = suite ("Exercise " ++ show (exerciseId ex)) $
   [ assertTrue "exercise terms defined" (not (null xs))
   , assertTrue "equivalence implemented" $
        let eq a b = equivalence ex (inContext ex a) (inContext ex b)
        in length (nubBy eq xs) > 1
   , assertTrue "similarity implemented" $
        let sim a b = similarity ex (inContext ex a) (inContext ex b)
        in length (nubBy sim xs) > 1
   , checkExamples qcgen ex
   ] ++
   case testGenerator ex of
      Nothing  -> []
      Just gen ->
         [ useProperty "parser/pretty printer" $ forAll showAsGen $
              checkParserPrettyEx ex . inContext ex . fromS
         , suite "Soundness non-buggy rules" $
                 let eq a b = equivalence ex (fromS a) (fromS b)
                     myGen  = showAs (prettyPrinterContext ex) (inContext ex <$> gen)
                     myView = makeView (Just . fromS) (S (prettyPrinterContext ex))
                     args   = stdArgs {maxSize = 10, maxSuccess = 10}
                 in [ usePropertyWith (showId r) args $
                         propRule eq (liftView myView r) myGen
                    | r <- ruleset ex
                    , not (isBuggy r)
                    ] {-
                        -}
         , useProperty "soundness strategy/generator" $
              forAll showAsGen $
                 maybe False (isReady ex) . fromContext
                 . applyD (strategy ex) . inContext ex . fromS
         ]
       where
         showAsGen = showAs (prettyPrinter ex) gen
 where
   rs = randomTerms qcgen ex Nothing
   xs | null rs   = examplesAsList ex
      | otherwise = take 10 rs

data ShowAs a = S {showS :: a -> String, fromS :: a}

instance Show (ShowAs a) where
   show a = showS a (fromS a)

showAs :: (a -> String) -> Gen a -> Gen (ShowAs a)
showAs f = fmap (S f)

-- check combination of parser and pretty-printer
checkParserPretty :: (a -> a -> Bool) -> (String -> Either String a) -> (a -> String) -> a -> Bool
checkParserPretty eq p pretty a =
   either (const False) (eq a) (p (pretty a))

checkParserPrettyEx :: Exercise a -> Context a -> Bool
checkParserPrettyEx ex ca =
   let f    = mapSecond make . parser ex
       make = setEnvironment (environment ca) . newContext . navigation ex
   in checkParserPretty (similarity ex) f (prettyPrinterContext ex) ca

propRule :: Show a => (a -> a -> Bool) -> Rule a -> Gen a -> Property
propRule eq r gen =
   forAll gen $ \a ->
   let xs = applyAll r a in
   not (null xs) ==>
   forAll (elements xs) $ \b ->
   a `eq` b

checkExamples :: QCGen -> Exercise a -> TestSuite
checkExamples qcgen ex
   | null xs   = mempty
   | otherwise = suite "Examples" $
        concatMap (checksForTerm True qcgen ex) xs
 where
   xs = examplesAsList ex

checksForTerm :: Bool -> QCGen -> Exercise a -> a -> [TestSuite]
checksForTerm leftMost _ ex a =
   concat
   -- Left-most derivation
      [ case defaultDerivation ex a of
           Just d  -> checksForDerivation ex d
           Nothing -> [assertTrue ("no derivation for " ++ prettyPrinter ex a) False]
      | leftMost
      ] {- ++
   case randomDerivation qcgen tree of
      Just d  -> checksForDerivation ex d
      Nothing -> []
 where
   tree = derivationTree (strategy ex) (inContext ex a) -}

checksForDerivation :: Exercise a -> Derivation (Rule (Context a), Environment) (Context a) -> [TestSuite]
checksForDerivation ex d =
   [ -- Conditions on starting term
     assertMessage "start term is suitable"
        (maybe False (isSuitable ex) (fromContext start))
        (prettyPrinterContext ex start)
   , assertMessage "start term is not ready"
        (maybe True (not . isReady ex) (fromContext start))
        (prettyPrinterContext ex start)
     -- Conditions on final term
   , assertMessage "final term is suitable"
        (maybe True (isSuitable ex) (fromContext final))
        (prettyPrinterContext ex start)
   , assertMessage "final term is ready"
        (maybe False (isReady ex) (fromContext final))
        (prettyPrinterContext ex start ++ "  =>  " ++
         prettyPrinterContext ex final)
     -- Parser/pretty printer on terms
   , assertNull "parser/pretty-printer" $ take 1 $ flip map (filter p1 ts) $ \hd ->
        let s = prettyPrinterContext ex hd
        in "parse error for " ++ s ++ ": parsed as "
        ++ either show (prettyPrinter ex) (parser ex s)
     -- Equivalences between terms
   , assertNull "equivalences" $ take 1 $ flip map (filter p2 pairs) $ \(x, y) ->
        "not equivalent: " ++ prettyPrinterContext ex x
        ++ "  with  " ++ prettyPrinterContext ex y
     -- Similarity of terms
   , let xs = filter p3 (triples d) in
     let (x, (r, _), y) = head xs in
     rateOnError 5 $ assertMessage "no similar steps"
        (null xs)
        ( prettyPrinterContext ex x ++
          "  with  " ++ prettyPrinterContext ex y ++
          "  using  " ++ show r)
   , assertNull "self similarity" $ take 1 $ do
        x <- terms d
        guard (not (similarity ex x x))
        return $ "term not similar to itself: " ++ prettyPrinterContext ex x
   , -- Parameters
     assertNull "parameters" $ take 1 $ do
        (r, env) <- steps d
        maybeToList (checkReferences r env)
   ]
 where
   start = firstTerm d
   final = lastTerm d
   ts  = terms d
   p1  = not . checkParserPrettyEx ex
   pairs     = [ (x, y) | x <- ts, y <- ts ]
   p2 (x, y) = not (equivalence ex x y)
   p3 (x, (_, _), y) = similarity ex x y &&
                       on (==) (maybe False (isReady ex) . fromContext) x y