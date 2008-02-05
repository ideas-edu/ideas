{-# OPTIONS -fglasgow-exts #-}
module Common.Assignment where

import Common.Transformation
import Common.Strategy hiding (not)
import Common.Utils
import Common.Unification
import System.Random
import Test.QuickCheck hiding (label, arguments)

data PackedAssignment = forall a . Pack { unpack :: Assignment a }

data Assignment a = Assignment
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
makeAssignment :: (Arbitrary a, Eq a, Show a) => Assignment a
makeAssignment = Assignment
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
   
randomTerm :: Assignment a -> IO a
randomTerm a = do 
   stdgen <- newStdGen
   return (randomTermWith stdgen a)

-- | Default size is 100
randomTermWith :: StdGen -> Assignment a -> a
randomTermWith stdgen a
   | finalProperty a term || not (suitableTerm a term) =
        randomTermWith (snd $ next stdgen) a
   | otherwise =
        term
 where
   term = generate 100 stdgen (generator a)

-- | Returns a text and the rule that is applicable
giveHint :: Assignment a -> [a] -> Maybe (Doc a, Rule a)
giveHint x = safeHead . giveHints x

-- | Returns a text and the rule that is applicable
giveHints :: Assignment a -> [a] -> [(Doc a, Rule a)]
giveHints x = map g . giveSteps x
 where
   g (x, y, _, _) = (x, y)
   
-- | Returns a text, a sub-expression that can be rewritten, and the result
-- | of the rewriting
giveStep :: Assignment a -> [a] -> Maybe (Doc a, Rule a, a, a)
giveStep x = safeHead . giveSteps x

giveSteps :: Assignment a -> [a] -> [(Doc a, Rule a, a, a)]
giveSteps x as = map g $ nextRulesForSequenceWith (equality x) (not . isMinorRule) (unlabel $ strategy x) as
 where
   g (rs, new) = 
      let r   = last rs
          old = applyListD (init rs) (last as)
          doc = text "Use rule " <> rule r <> 
                case arguments r old of
                   Just args -> text "\n   with arguments " <> text args
                   Nothing   -> emptyDoc
      in (doc, r, old, new)

-- | The strategy in the assignment should reflect the current position in the 
-- | strategy, which might not be the original (complete) strategy.
feedback :: Assignment a -> [a] -> String -> Feedback a
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
                    _ -> Correct (text "Equivalent, but not a known rule. Please retry.") Nothing
         
stepsRemaining :: Assignment a -> a -> Int
stepsRemaining x a = 
   case runStrategyRules (unlabel $ strategy x) a of
      (rs, _):_ -> length (filter (not . isMinorRule) rs)
      _         -> 0


data Feedback a = SyntaxError (Doc a) (Maybe a) {- corrected -}
                | Incorrect   (Doc a) (Maybe a)
                | Correct     (Doc a) (Maybe (Rule a)) {- The rule that was applied -}

getRuleNames :: Assignment a -> [String]
getRuleNames = map name . ruleset

---------------------------------------------------------------
-- Documents (feedback with structure)
                
newtype Doc a = D [DocItem a]

data DocItem a = Text String | Term a | DocRule (Rule a)
           
instance Show a => Show (Doc a) where
   show = showDocWith show

emptyDoc :: Doc a
emptyDoc = D []

showDoc :: Assignment a -> Doc a -> String
showDoc = showDocWith . prettyPrinter

showDocWith :: (a -> String) -> Doc a -> String
showDocWith f (D xs) = concatMap g xs
 where
   g (Text s)    = s
   g (Term a)    = f a 
   g (DocRule r) = name r
   
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
rule r = D [DocRule r]

---------------------------------------------------------------
-- Checks for an assignment

-- | An instance of the Arbitrary type class is required because the random
-- | term generator that is part of an Assignment is not used for the checks:
-- | the terms produced by this generator will typically be biased.


checkAssignment :: (Arbitrary a, Show a) => Assignment a -> IO ()
checkAssignment = checkAssignmentWith checkRule

checkAssignmentSmart :: (Arbitrary a, Show a, Substitutable a) => Assignment a -> IO ()
checkAssignmentSmart = checkAssignmentWith checkRuleSmart

checkAssignmentWith :: (Arbitrary a, Show a) => ((a -> a -> Bool) -> Rule a -> IO b) -> Assignment a -> IO ()
checkAssignmentWith f a = do
   putStrLn ("Checking assignment: " ++ shortTitle a)
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