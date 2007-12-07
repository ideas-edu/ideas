{-# OPTIONS -XTypeSynonymInstances #-}
module Common.Assignment where

import Common.Transformation
import Common.Strategy
import Common.Utils
import System.Random
import Test.QuickCheck

data Assignment a = Assignment
   { parser        :: String -> Either (Doc a, Maybe a) a
   , prettyPrinter :: a -> String
   , equivalence   :: a -> a -> Bool
   , equality      :: a -> a -> Bool -- syntactic equality
   , finalProperty :: a -> Bool
   , ruleset       :: [Rule a]
   , strategy      :: Strategy a
   , generator     :: Gen a
   , configuration :: Configuration
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
randomTermWith stdgen = generate 100 stdgen . generator

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
                    (rs, _, _):_ -> Correct (text "Well done! You applied rule " <> rule (last rs)) True
                    _    -> Correct (text "Equivalent, but not a known rule. Please retry.") False
         
stepsRemaining :: Assignment a -> a -> Int
stepsRemaining x a = 
   case intermediates (strategy x) a of
      (rs, _, _):_ -> length (filter (not . isMinorRule) rs)
      _            -> 0

data Feedback a = SyntaxError (Doc a) (Maybe a) {- corrected -}
                | Incorrect   (Doc a) (Maybe a)
                | Correct     (Doc a) Bool
                
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
