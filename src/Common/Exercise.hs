{-# OPTIONS -fglasgow-exts #-}
module Common.Exercise where

import Common.Apply
import Common.Transformation
import Common.Strategy hiding (not)
import Common.Utils
import Common.Unification
import System.Random
import Test.QuickCheck hiding (label, arguments)

data Exercise a = Exercise
   { shortTitle    :: String
   , parser        :: String -> Either (Doc a, Maybe a) a
   , prettyPrinter :: a -> String
   , equivalence   :: a -> a -> Bool
   , equality      :: a -> a -> Bool -- syntactic equality
   , finalProperty :: a -> Bool
   , ruleset       :: [Rule a]
   , strategy      :: LabeledStrategy a
   , generator     :: Gen a
   , suitableTerm  :: a -> Bool
   , configuration :: Configuration
   }

-- default values for all fields
makeExercise :: (Arbitrary a, Eq a, Show a) => Exercise a
makeExercise = Exercise
   { shortTitle    = "no short title"
   , parser        = const $ Left (text "no parser", Nothing)
   , prettyPrinter = show
   , equivalence   = (==)
   , equality      = (==)
   , finalProperty = const True
   , ruleset       = []
   , strategy      = label "Succeed" succeed
   , generator     = arbitrary
   , suitableTerm  = const True
   , configuration = defaultConfiguration
   }

data Language = English | Dutch

data Configuration = Configuration
   { language :: Language
   }

defaultConfiguration :: Configuration
defaultConfiguration = Configuration
   { language = English
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

{-
-- | Returns a text and the rule that is applicable
giveHint :: Exercise a -> [a] -> Maybe (Doc a, Rule a)
giveHint x = safeHead . giveHints x

-- | Returns a text and the rule that is applicable
giveHints :: Exercise a -> [a] -> [(Doc a, Rule a)]
giveHints x = map g . giveSteps x
 where
   g (x, y, _, _) = (x, y)
   
-- | Returns a text, a sub-expression that can be rewritten, and the result
-- | of the rewriting

giveStep :: Exercise a -> [a] -> Maybe (Doc a, Rule a, a, a)
giveStep x = safeHead . giveSteps x

giveSteps :: Exercise a -> [a] -> [(Doc a, Rule a, a, a)]
giveSteps x as = map g $ nextRulesForSequenceWith (equality x) (not . isMinorRule) (unlabel $ strategy x) as
 where
   g (rs, new) = 
      let r   = last rs
          old = applyListD (init rs) (last as)
          doc = text "Use rule " <> rule r <> 
                case arguments r old of
                   Just args -> text "\n   with arguments " <> text args
                   Nothing   -> emptyDoc
      in (doc, r, old, new) -}

-- | Returns a text and the rule that is applicable
giveHintNew :: Exercise a -> Prefix -> a -> Maybe (Doc a, Rule a)
giveHintNew ex p = safeHead . giveHintsNew ex p

-- | Returns a text and the rule that is applicable
giveHintsNew :: Exercise a -> Prefix -> a -> [(Doc a, Rule a)]
giveHintsNew ex p = map g . giveStepsNew ex p
 where
   g (x, y, _, _, _) = (x, y)

giveStepNew :: Exercise a -> Prefix -> a -> Maybe (Doc a, Rule a, Prefix, a, a)
giveStepNew ex p = safeHead . giveStepsNew ex p

giveStepsNew :: Exercise a -> Prefix -> a -> [(Doc a, Rule a, Prefix, a, a)]
giveStepsNew ex p0@(P xs) a = 
   case runPrefix p0 (strategy ex) of
      Nothing -> []
      Just (_, g) -> 
         let make (new, p1) = 
                let prefix = plusPrefix p0 p1 
                    steps  = maybe [] fst $ runPrefix prefix $ strategy ex
                    minors = stepsToRules (drop (length xs) steps)
                    old    = if null minors then a else applyListD (init minors) a
                in case lastRuleInPrefix prefix (strategy ex) of
                      Just r -> [(doc r old, r, prefix, old, new)]
                      _      -> []
             stopC (Major _ _) = True
             stopC _           = False
             doc r old = text "Use rule " <> rule r <> 
                case arguments r old of
                   Just args -> text "\n   with arguments " <> text args
                   Nothing   -> emptyDoc
         in concatMap make $ runGrammarUntil stopC a g
      
{-
-- | The strategy in the exercise should reflect the current position in the 
-- | strategy, which might not be the original (complete) strategy.
feedback :: Exercise a -> [a] -> String -> Feedback a
feedback x as txt =
   case parser x txt of
      Left (msg, suggestion) -> 
         SyntaxError msg suggestion
      Right new
         | not (equivalence x (last as) new) -> 
              Incorrect (text "Incorrect") Nothing -- no suggestion yet
         | otherwise -> 
              let paths = nextRulesForSequenceWith (equality x) (not . isMinorRule) (unlabel $ strategy x) as
                  check = equality x new . snd
              in case filter check paths of
                    (rs, _):_ -> Correct (text "Well done! You applied rule " <> rule (last rs)) (Just (last rs))
                    _ | equality x (last as) new -> 
                         Correct (text "You have submitted the current term.") Nothing
                    _ -> Correct (text "Equivalent, but not a known rule. Please retry.") Nothing -}
                    
         
feedbackNew :: Exercise a -> Prefix -> a -> String -> Feedback a
feedbackNew ex p0 a txt =
   case parser ex txt of
      Left (msg, suggestion) -> 
         SyntaxError msg suggestion
      Right new
         | not (equivalence ex a new) -> 
              Incorrect (text "Incorrect") Nothing -- no suggestion yet
         | otherwise -> 
              let answers = giveStepsNew ex p0 a
                  check (_, _, _, _, this) = equality ex new this
              in case filter check answers of
                    (_, r, newPrefix, _, newTerm):_ -> Correct (text "Well done! You applied rule " <> rule r) (Just (newPrefix, r, newTerm))
                    _ | equality ex a new -> 
                         Correct (text "You have submitted the current term.") Nothing
                    _ -> Correct (text "Equivalent, but not a known rule. Please retry.") Nothing
              
stepsRemaining :: Strategy a -> a -> Int
stepsRemaining s a =
   case runStrategyRules s a of
      (rs, _):_ -> length (filter (not . isMinorRule) rs)
      _         -> 0

stepsRemainingA :: Exercise a -> [a] -> Int
stepsRemainingA a as =
   stepsRemaining (remainingStrategy (equality a) (not . isMinorRule) (unlabel $ strategy a) as) (last as)

data Feedback a = SyntaxError (Doc a) (Maybe a) {- corrected -}
                | Incorrect   (Doc a) (Maybe a)
                | Correct     (Doc a) (Maybe (Prefix, Rule a, a)) {- The rule that was applied -}

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

instance Show a => Show (Doc a) where
   show = showDocWith show

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