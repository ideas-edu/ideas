{-# OPTIONS -fglasgow-exts #-}
module Common.Assignment where

import Common.Transformation
import Common.Strategy
import Common.Utils
import Common.Unification
import System.Random
import Test.QuickCheck

data PackedAssignment = forall a . Pack { unpack :: Assignment a }

data Assignment a = Assignment
   { shortTitle    :: String
   , parser        :: String -> Either (Doc a, Maybe a) a
   , prettyPrinter :: a -> String
   , equivalence   :: a -> a -> Bool
   , equality      :: a -> a -> Bool -- syntactic equality
   , finalProperty :: a -> Bool
   , ruleset       :: [Rule a]
   , strategy      :: Strategy a
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
   , strategy      = succeed
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
giveHint :: Assignment a -> a -> (Doc a, Rule a)
giveHint x a =
   case giveHints x a of
      hd:_ -> hd
      _    -> (emptyDoc, idRule)

-- | Returns a text and the rule that is applicable
giveHints :: Assignment a -> a -> [(Doc a, Rule a)]
giveHints x = map g . nextRulesWith (not . isMinorRule) (strategy x)
 where
   g (rs, a, s) = (rule (last rs), last rs) -- ignores rest strategy
   
-- | Returns a text, a sub-expression that can be rewritten, and the result
-- | of the rewriting
giveStep :: Assignment a -> a -> (Doc a, a, a)
giveStep x a = 
   case giveSteps x a of
      hd:_ -> hd
      _    -> (emptyDoc, a, a)

giveSteps :: Assignment a -> a -> [(Doc a, a, a)]
giveSteps x a = map g $ nextRulesWith (not . isMinorRule) (strategy x) a
 where
   g (rs, b, _) = (rule (last rs), applyListD (init rs) a, b)

feedback :: Assignment a -> a -> String -> Feedback a
feedback x a txt =
   case parser x txt of
      Left (msg, suggestion) -> 
         SyntaxError msg suggestion
      Right new
         | not (equivalence x a new) -> 
              Incorrect (text "Incorrect") Nothing -- no suggestion yet
         | otherwise -> 
              let paths = nextRulesWith (not . isMinorRule) (strategy x) a 
                  check = equality x new . snd3
              in case filter check paths of
                    (rs, _, _):_ -> Correct (text "Well done! You applied rule " <> rule (last rs)) (Just (last rs))
                    _    -> Correct (text "Equivalent, but not a known rule. Please retry.") Nothing
         
stepsRemaining :: Assignment a -> a -> Int
stepsRemaining x a = 
   case runStrategyRules (strategy x) a of
      (rs, _):_ -> length (filter (not . isMinorRule) rs)
      _         -> 0


data Feedback a = SyntaxError (Doc a) (Maybe a) {- corrected -}
                | Incorrect   (Doc a) (Maybe a)
                | Correct     (Doc a) (Maybe (Rule a)) {- The rule that was applied -}

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